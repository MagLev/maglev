#include <string.h>
#include <assert.h>

#include <yaml.h>
#include "parser.h"

/* void my_handle_event(yaml_event_t *event); */
void set_event_flag(parser_event_t *event, u_char flag) {
  event->flag |= flag;
}
u_char get_event_flag(parser_event_t *event, u_char flag) {
  return (event->flag & flag) != 0;
}

void free_parser_context(parser_context_t *context) {
  assert(context);
  free(context);
}

parser_event_t *create_event() {
  return (parser_event_t *)malloc(sizeof(parser_event_t));
}

/* Frees all allocated pointers in event; does not free event itself. */
void free_event(parser_event_t *event) {
  assert(event);

  /* The actual tag_directives are freed by libyaml; we just free the array */
  if (event->tag_directives)
    free(event->tag_directives);
}

void invalidate_parser(parser_context_t *parser_context) {
  assert(IS_VALID_PARSER_CONTEXT(parser_context));
  yaml_parser_delete(&(parser_context->parser));
  parser_context->parser_validp = 0;
}

/*
 * Set the state of the parser event from information in the yaml event.
 */
void set_parser_event(parser_event_t *p_event, yaml_event_t *event) {

  /* RxINC: When/were should we free p_event alloc memory? */
  memset(p_event, 0, sizeof(parser_event_t));

  /* for non-errors, parser events and yaml parser events map directly */
  p_event->type = event->type;

  switch(event->type) {
  case YAML_STREAM_START_EVENT:
    p_event->encoding = event->data.stream_start.encoding;
    break;

  case YAML_STREAM_END_EVENT:
    /* Nothing */
    break;

  case YAML_DOCUMENT_START_EVENT:
    /* Record implicit flag */
    if (event->data.document_start.implicit == 1) {
      set_event_flag(p_event, IMPLICIT_FLAG);
    }

    /* Encode document version info */
    if (event->data.document_start.version_directive) {
      set_event_flag(p_event, HAS_VERSION_FLAG);
      p_event->version_major = event->data.document_start.version_directive->major;
      p_event->version_minor = event->data.document_start.version_directive->minor;
    }

    /* Encode tag directives */
    if (event->data.document_start.tag_directives.start) {
      yaml_tag_directive_t *start = event->data.document_start.tag_directives.start;
      yaml_tag_directive_t *end   = event->data.document_start.tag_directives.end;
      int num_tags = ((end - start)/sizeof(yaml_tag_directive_t *)) + 1;

      p_event->num_tags = num_tags;
      p_event->tag_directives = calloc(2*num_tags, sizeof(char *));
      char *current_tag = p_event->tag_directives;
      for(; start != end; start++) {
        *current_tag++ = start->handle;
        *current_tag++ = start->prefix;
        fprintf(stderr, "Tag handle: '%s'\n", start->handle);
        fprintf(stderr, "Tag prefix: '%s'\n", start->prefix);
      }
    }
    break;

  case YAML_DOCUMENT_END_EVENT:
    /* Nothing */
    break;

  case YAML_ALIAS_EVENT:
    p_event->anchor = event->data.alias.anchor;
    break;

  case YAML_SCALAR_EVENT:
    p_event->scalar = event->data.scalar.value;
    p_event->scalar_length = (long)event->data.scalar.length;
    p_event->anchor = event->data.scalar.anchor;
    p_event->tag = event->data.scalar.tag;
    if (event->data.scalar.plain_implicit) {
      set_event_flag(p_event, PLAIN_IMPLICIT_FLAG);
    }
    if (event->data.scalar.quoted_implicit) {
      set_event_flag(p_event, QUOTED_IMPLICIT_FLAG);
    }
    p_event->style = event->data.scalar.style;
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
    fprintf(stderr, "UNRECOGNIZED EVENT: %d\n", event->type);
    break;
  }
}

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

parser_event_t *next_event(parser_context_t *parser_context,
                           parser_event_t *event) {
  assert(IS_VALID_PARSER_CONTEXT(parser_context));

  yaml_event_t libyaml_event;
  yaml_parser_t *parser = &(parser_context->parser);

  if (!yaml_parser_parse(parser, &libyaml_event)) {
      fprintf(stderr, "PARSE ERROR\n");

      event->type = PARSE_ERROR_EVENT;
      event->yaml_line   = parser->mark.line;
      event->yaml_column = parser->mark.column;

      invalidate_parser(parser_context);

      return event;
  }

  set_parser_event(event, &libyaml_event);

  yaml_event_delete(&libyaml_event);
  return event;
}


/*
 * Allocate a new parser context and initialize it.  The parser context
 * holds the libyaml parser and the input string.
 */
parser_context_t *create_parser_context(const char *input) {
  parser_context_t *parser_context = malloc(sizeof(parser_context_t));
  memset(parser_context, 0, sizeof(parser_context_t));
  size_t length = strlen(input);
  yaml_parser_t *parser = &(parser_context->parser);

  yaml_parser_initialize(parser);
  yaml_parser_set_input_string(parser, (unsigned char *)input, length);
  parser_context->input = input; /* should I make a copy? */
  parser_context->parser_validp = 1;
  return parser_context;
}
