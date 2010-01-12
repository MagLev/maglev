#include <string.h>
#include <assert.h>

#include <yaml.h>
#include "parser.h"

/* void my_handle_event(yaml_event_t *event); */

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
  yaml_parser_set_input_string(parser, input, length);
  parser_context->input = input; /* should I make a copy? */
  parser_context->parser_validp = 1;
  return parser_context;
}

void free_parser_context(parser_context_t *context) {
  assert(context);
  free(context);
}

void free_event(parser_event_t *event) {
  assert(event);
  /* TODO: need to free any allocated stuff */
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

void invalidate_parser(parser_context_t *parser_context) {
  assert(IS_VALID_PARSER_CONTEXT(parser_context));
  yaml_parser_delete(&(parser_context->parser));
  parser_context->parser_validp = 0;
}

/*
 * Set the state of the parser event from information in the yaml event.
 */
void set_parser_event(parser_event_t *p_event, yaml_event_t *event) {

  memset(p_event, 0, sizeof(parser_event_t));
  /* for non-errors, parser events and yaml parser events map directly */
  p_event->type = event->type;

  switch(event->type) {
  case YAML_STREAM_START_EVENT:
    p_event->encoding = event->data.stream_start.encoding;
    break;

  case YAML_STREAM_END_EVENT:
    /* TODO: */
    break;

  case YAML_DOCUMENT_START_EVENT:
    /* Encode document version info */
    if (event->data.document_start.version_directive) {
      p_event->version_major = event->data.document_start.version_directive->major;
      p_event->version_minor = event->data.document_start.version_directive->minor;
    }
    /* Encode tag directives */
    if (event->data.document_start.tag_directives.start) {
      int num_tags =
        (event->data.document_start.tag_directives.end -
         event->data.document_start.tag_directives.start) / sizeof(yaml_tag_directive_t *);
      fprintf(stderr, "num_tags: %d (%d)\n", num_tags,         (event->data.document_start.tag_directives.end -
         event->data.document_start.tag_directives.start) );
      /* TODO: */
    }
    
    break;

  case YAML_DOCUMENT_END_EVENT:
    /* TODO: */
    break;

  case YAML_ALIAS_EVENT:
    /* TODO: */
    break;

  case YAML_SCALAR_EVENT:
    /* TODO: */
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
