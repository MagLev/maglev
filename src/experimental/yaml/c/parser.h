/*
 * Header file for the dynamic parser
 */

/*
  TODO:  Should any of this be extern or static?
 */

#ifndef __PBM_PARSER_H
#define __PBM_PARSER_H

#include <sys/types.h>
#include <yaml.h>

#define IS_VALID_PARSER_CONTEXT(context)         \
  (context != NULL && (context->parser_validp))

typedef enum parser_character_encoding_e {
  ANY, UTF8, UTF_16LE, UTF_16BE
} parser_character_encoding_t;

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
  yaml_char_t *input;
} parser_context_t;


/*
 * TODO: Make a union, once I've figured out all of the data
 * that gets passed, and who passes it.
 *
 * NOTE: yaml_char_t is unsigned char
 */
typedef struct parser_event_s {
  parser_event_type_t type;
  int encoding;

  int version_major;
  int version_minor;

  int  num_tags;
  yaml_char_t **tag_directives;

  size_t yaml_line;
  size_t yaml_column;

  yaml_char_t *scalar;
  long  scalar_length;

  long style;

  yaml_char_t *anchor;
  yaml_char_t *tag;
  u_char flag;
} parser_event_t;
/* #define VERSION_FLAG 0x01 */
/* #define HAS_VERSION(event) \ */
/*   (event->flags & VERSION_FLAG) != 0 */

#define HAS_VERSION_FLAG     (1 << 0)  /* 0x01 */
#define IMPLICIT_FLAG        (1 << 1)  /* 0x02 */
#define QUOTE_FLAG           (1 << 2)  /* 0x04 */
#define PLAIN_IMPLICIT_FLAG  (1 << 3)  /* 0x08 */
#define QUOTED_IMPLICIT_FLAG (1 << 4)  /* 0x10 */

void set_event_flag(parser_event_t *event, u_char flag);
u_char get_event_flag(parser_event_t *event, u_char flag);

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
