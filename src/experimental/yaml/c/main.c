
#include "parser.h"

int main(int argc, char **argv) {

  parser_context_t *parser_context = parser_new();
  const unsigned char *input = "--- a";

  parse(parser_context, input);

  return 0;
}

