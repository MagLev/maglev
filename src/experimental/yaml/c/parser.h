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

#define VALIDP 0xef23
#define IS_VALID_PARSER_CONTEXT(context)         \
  ((context != NULL) && ((context->parser_validp) == VALIDP))


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

/*
 * This is the simplified version of the information in a yaml_event_t.
 * This is the only information the psych handler depends on.
 *
 * TODO: Make a union, once I've figured out all of the data that gets
 * passed, and who passes it.
 * NOTE: yaml_char_t is unsigned char
 */
typedef struct parser_event_s {
  parser_event_type_t type;
  int encoding;

  int version_major;
  int version_minor;

  int  num_tags;
  yaml_char_t **tag_directives;

  /* Info about the position of the current token in input */
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

/*
 * Each ruby Parser object has one parser_context_t to hold its parsing
 * state.  This includes the libyaml parser
 */
typedef struct parser_context_s {
  yaml_parser_t parser;        /* The libyaml parser        */
  yaml_event_t  event;         /* the current libyaml event */
  parser_event_t psych_event;  /* digested version of event */
  yaml_char_t *input;          /* Not needed?? debug only?  */
  int parser_validp;
} parser_context_t;

/*
 * API Calls
 */

/* Create a new parser context.  This contains the parser */
parser_context_t *create_parser_context(unsigned char *input);

/*
 * Get the next event for the parser context. The data in the returned
 * parser_event_t is good until the next call to next_event().  All data
 * stored in the event_t is released at the beginning of the next call to
 * next_event().  All data in the event is freed when the parser_context is
 * freed via a call to free_parser_context_event().
 */
parser_event_t *next_event(parser_context_t *parser_context);

/* Free memory associated with the parser context */
void free_parser_context_event(parser_context_t *parser_context);

/*
 * API: Ruby calls this
 * version_info should be pointer to memory to hold 3 ints.
 * The major, minor and patch level of lib yaml will be returned.
 */
void libyaml_version(int version_info[]);
#endif
