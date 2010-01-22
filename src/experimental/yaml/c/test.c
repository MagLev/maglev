#include <stdlib.h>
#include "test.h"

parser_t *test_version_data() {
  parser_t *parser = malloc(sizeof(parser_t));
  memset(parser, 0, sizeof(parser_t));

  parser->type = STREAM_START_EVENT;
  parser->data.version_directive.major = 2;
  parser->data.version_directive.minor = 3;

  return parser;
}

parser_t *test_scalar_data() {
  char *orig_str = "Some data to copy";
  parser_t *parser = (parser_t *)malloc(sizeof(parser_t));
  memset(parser, 0, sizeof(parser_t));

  parser->type = SCALAR_EVENT;
  parser->data.scalar.value = (char *)malloc(strlen(orig_str)+1);
  strcpy(parser->data.scalar.value, orig_str);
  parser->data.scalar.length = strlen(orig_str);

  return parser;
}
