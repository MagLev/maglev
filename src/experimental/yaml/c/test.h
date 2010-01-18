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

typedef struct parser_s {
  parser_event_type_t type;
  union {
    struct {
      char *value;
      size_t length;
    } scalar;

    struct {
      int major;
      int minor;
    } version_directive;
  } data;
} parser_t;
