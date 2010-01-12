
#include "parser.h"

int main(int argc, char **argv) {


  /*
    #   %YAML 1.1
    #   %TAG ! tag:gemstone.com,2009:
    #   --- !squee
    
    const unsigned char *input = "--- a";

    const unsigned char *input = "%YAML 1.1\n%TAG ! tag:gemstone.com,2009:\n--- !squee\n";

  */
  const unsigned char *input =
    "%YAML 1.1\n%TAG ! tag:gemstone.com,2009:\n--- !squee\n";
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

