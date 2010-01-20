#include <unistd.h>
#include <string.h>
#include <assert.h>

#include <yaml.h>
#include "parser.h"

void copy_event(parser_event_t *psych_event, yaml_event_t *yaml_event);

/* void my_handle_event(yaml_event_t *event); */
void set_event_flag(parser_event_t *event, u_char flag) {
  event->flag |= flag;
}

/* RxINC: Not used?? */
u_char get_event_flag(parser_event_t *event, u_char flag) {
  return (event->flag & flag) != 0;
}

/* Clean up allocated memory in the interior structs. */
void free_event_data(parser_context_t *parser_context) {
  if (IS_VALID_PARSER_CONTEXT(parser_context)) {
    /* Free libyaml structs */
    yaml_event_delete(&(parser_context->event));

    /* Free data held in the parser_event_t */

    /* The actual tag_directives are freed by libyaml; we just free the array */
    parser_event_t *event = &(parser_context->psych_event);
    if (event->tag_directives) {
      free(event->tag_directives);
    }

    memset(&(parser_context->psych_event), 0, sizeof(parser_event_t));
  }
}

/* API: Called from Ruby */
void free_parser_context(parser_context_t *context) {
  fflush(stderr);
  fprintf(stderr, "+++ free_parser_context\n");
  fflush(stderr);
  assert(context);
  free(context);
}

void pause_for_debug() {
  fprintf(stderr, "+++ PID %d pausing for debugger\n", getpid());
  fflush(stderr);
  int wait_for_debug = 1;
  while (wait_for_debug) {
    sleep(2);
  }
}

void invalidate_parser(parser_context_t *parser_context) {
  fprintf(stderr, "+++ invalidate_parser\n");
  fflush(stderr);
  assert(IS_VALID_PARSER_CONTEXT(parser_context));
  free_event_data(parser_context);
  parser_context->parser_validp = 0;
}

/* API: Called from Ruby */
parser_event_t *next_event(parser_context_t *parser_context) {
  assert(IS_VALID_PARSER_CONTEXT(parser_context));

  yaml_event_t *libyaml_event = &(parser_context->event);
  yaml_parser_t *parser = &(parser_context->parser);
  parser_event_t *result = &(parser_context->psych_event);

  free_event_data(parser_context);

  if (!yaml_parser_parse(parser, libyaml_event)) {
      fprintf(stderr, "+++ PARSE ERROR\n");

      /* For GDB
      size_t line = parser->mark.line;
      size_t column = parser->mark.column;
      */
      pause_for_debug();

      result->type = PARSE_ERROR_EVENT;
      result->yaml_line   = parser->mark.line;
      result->yaml_column = parser->mark.column;

      /* We have copied only integers out of the event, so safe to delete */
      yaml_event_delete(libyaml_event);
      invalidate_parser(parser_context);
      return result;
  }
  copy_event(result, libyaml_event);
  return result;
}


/*
 * API: Called from Ruby.
 *
 * Allocate a new parser context and initialize it.  The parser context
 * holds the libyaml parser, the yaml event struct used for this parser and
 * the input string.
 */
parser_context_t *create_parser_context(unsigned char *input) {
  parser_context_t *parser_context = malloc(sizeof(parser_context_t));
  memset(parser_context, 0, sizeof(parser_context_t));

  size_t length = strlen((char *)input);    /* RxINC */

  yaml_parser_t *parser = &(parser_context->parser);
  yaml_parser_initialize(parser);
  yaml_parser_set_input_string(parser, (unsigned char *)input, length);

  parser_context->input = input; /* should I make a copy? */
  parser_context->parser_validp = VALIDP;
  return parser_context;
}



/*
 * This copies relevant state out of the yaml_event, simplifies it and
 * stores state into the psych_event.  This simplified state is all that is
 * needed by the Psych code.  Assumes the psych_event is already cleared.
 */
void copy_event(parser_event_t *psych_event, yaml_event_t *yaml_event) {

  /* the psych_event is already memset() to 0 */

  /* for non-errors, parser events and yaml parser events map directly */
  psych_event->type = yaml_event->type;

  switch(yaml_event->type) {
  case YAML_STREAM_START_EVENT:
    psych_event->encoding = yaml_event->data.stream_start.encoding;
    break;

  case YAML_STREAM_END_EVENT:
    /* Nothing */
    break;

  case YAML_DOCUMENT_START_EVENT:
    /* Record implicit flag */
    if (yaml_event->data.document_start.implicit == 1) {
      set_event_flag(psych_event, IMPLICIT_FLAG);
    }

    /* Encode document version info */
    if (yaml_event->data.document_start.version_directive) {
      set_event_flag(psych_event, HAS_VERSION_FLAG);
      psych_event->version_major = yaml_event->data.document_start.version_directive->major;
      psych_event->version_minor = yaml_event->data.document_start.version_directive->minor;
    }

    /* Encode tag directives */
    if (yaml_event->data.document_start.tag_directives.start) {
      yaml_tag_directive_t *start = yaml_event->data.document_start.tag_directives.start;
      yaml_tag_directive_t *end   = yaml_event->data.document_start.tag_directives.end;
      int num_tags = ((end - start)/sizeof(yaml_tag_directive_t *)) + 1;

      psych_event->num_tags = num_tags;
      psych_event->tag_directives = (yaml_char_t **)calloc(2*num_tags, sizeof(yaml_char_t *));
      yaml_char_t **current_tag = psych_event->tag_directives;
      for(; start != end; start++) {
        *current_tag++ = start->handle;
        *current_tag++ = start->prefix;
      }
      fflush(stderr);
      current_tag = psych_event->tag_directives;
      fprintf(stderr, "+++ current_tag[0]: '%s'\n", current_tag[0]);
      fprintf(stderr, "+++ current_tag[1]: '%s'\n", current_tag[1]);
    }
    break;

  case YAML_DOCUMENT_END_EVENT:
    /* Nothing */
    break;

  case YAML_ALIAS_EVENT:
    psych_event->anchor = yaml_event->data.alias.anchor;
    break;

  case YAML_SCALAR_EVENT:
    psych_event->scalar = yaml_event->data.scalar.value;
    psych_event->scalar_length = (long)yaml_event->data.scalar.length;
    psych_event->anchor = yaml_event->data.scalar.anchor;
    psych_event->tag = yaml_event->data.scalar.tag;
    if (yaml_event->data.scalar.plain_implicit) {
      set_event_flag(psych_event, PLAIN_IMPLICIT_FLAG);
    }
    if (yaml_event->data.scalar.quoted_implicit) {
      set_event_flag(psych_event, QUOTED_IMPLICIT_FLAG);
    }
    psych_event->style = yaml_event->data.scalar.style;
    break;

  case YAML_SEQUENCE_START_EVENT:
    /* TODO: */
    break;

  case YAML_SEQUENCE_END_EVENT:
    /* TODO: */
    break;

  case YAML_MAPPING_START_EVENT:
    /* TODO: */
    break;

  case YAML_MAPPING_END_EVENT:
    /* TODO: */
    break;

  case YAML_NO_EVENT:
    /* TODO: */
    break;

  default:
    fprintf(stderr, "+++ UNRECOGNIZED EVENT: %d\n", yaml_event->type);
    break;
  }
}

/* Unused ? */
char *event_name_for(const parser_event_type_t type) {
  switch(type) {
  case NO_EVENT:             return "NO_EVENT";
  case STREAM_START_EVENT:   return "STREAM_START_EVENT";
  case STREAM_END_EVENT:     return "STREAM_END_EVENT";
  case DOCUMENT_START_EVENT: return "DOCUMENT_START_EVENT";
  case DOCUMENT_END_EVENT:   return "DOCUMENT_END_EVENT";
  case ALIAS_EVENT:          return "ALIAS_EVENT";
  case SCALAR_EVENT:         return "SCALAR_EVENT";
  case SEQUENCE_START_EVENT: return "SEQUENCE_START_EVENT";
  case SEQUENCE_END_EVENT:   return "SEQUENCE_END_EVENT";
  case MAPPING_START_EVENT:  return "MAPPING_START_EVENT";
  case MAPPING_END_EVENT:    return "MAPPING_END_EVENT";
  default:                   return "NOT AN EVENT";
  }
}
