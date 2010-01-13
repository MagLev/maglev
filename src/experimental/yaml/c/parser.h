/*
 * Header file for the dynamic parser
 */

/*
  TODO:  Should any of this be extern or static?
 */

#ifndef __PBM_PARSER_H
#define __PBM_PARSER_H

#include <yaml.h>

#define IS_VALID_PARSER_CONTEXT(context)         \
  (context != NULL && (context->parser_validp))

/*
typedef enum parser_character_endocing_e {
  ANY, UTF8, UTF_16LE, UTF_16BE
} parser_character_endocing_t;
*/

typedef enum parser_event_type_e {
  /*
   * NOTE: The first eleven values are the same as
   * the yaml_event_type_e values
   */
  NO_EVENT,

  STREAM_START_EVENT,
  STREAM_END_EVENT,

  DOCUMENT_START_EVENT,
  DOCUMENT_END_EVENT,

  ALIAS_EVENT,
  SCALAR_EVENT,

  SEQUENCE_START_EVENT,
  SEQUENCE_END_EVENT,

  MAPPING_START_EVENT,
  MAPPING_END_EVENT,
  /* END yaml_event_type_e */

  PARSE_ERROR_EVENT
} parser_event_type_t;

typedef struct parser_context_s {
  yaml_parser_t parser;
  int parser_validp;
  char *input;
} parser_context_t;


/*
 * TODO: Make a union, once I've figured out all of the data
 * that gets passed, and who passes it.
 */
typedef struct parser_event_s {
  parser_event_type_t type;

  int encoding;

  int version_major;
  int version_minor;

  size_t yaml_line;     /* Error info */
  size_t yaml_column;   /* Error info */
} parser_event_t;

int parse(parser_context_t *parser_context);

/* Create a new parser context.  This contains the parser */
parser_context_t *create_parser_context();

/* Create a new event struct. */
parser_event_t *create_event();

/* Get the next event for the parser context; modifies *event */
parser_event_t *next_event(parser_context_t *parser_context,
                           parser_event_t *event);


/* Get a string name for the event type */
char *event_name_for(const parser_event_type_t type);

#endif
