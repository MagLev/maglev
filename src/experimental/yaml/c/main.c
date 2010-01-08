
#include "parser.h"

int main(int argc, char **argv) {

  const unsigned char *input = "--- a";
  fprintf(stderr, "-- main: A: about to create parser\n");
  parser_context_t *parser_context = create_parser_context(input);

  int done = 0;
  parser_event_t event;
  while (! done) {
    next_event(parser_context, &event);
    fprintf(stderr, "-- main: %s\n", event_name_for(event.type));
    done = (event.type == STREAM_END_EVENT);
  }

  free_parser_context(parser_context);
  return 0;
}

