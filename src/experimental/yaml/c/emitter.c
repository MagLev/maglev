#include <yaml.h>

/*
 * The recipe for working with the libyaml emitter is:
 *
 * 1: Create the appropriate yaml_event_t, by calling the appropriate
 *    yaml_*_event_initialize() function.
 * 2: Call yaml_emitter_emit()
 * 3: Return the result from yaml_emitter_emit()
 *
 * Note: yaml_emitter_emit() does all the clean up of the event, so we
 * don't need to worry about that.
 */

/*
 * API: Ruby calls this
 *
 * Creates the fully initialized libyaml emitter object used by one
 * instance of Psych::Emitter.
 */
yaml_emitter_t *create_emitter() {
  yaml_emitter_t *emitter = (yaml_emitter_t *)malloc(sizeof(yaml_emitter_t));
  yaml_emitter_initialize(emitter);
  yaml_emitter_set_unicode(emitter, 1);
  return emitter;
}

/* void emit(yaml_emitter_t *emitter, yaml_event_t *event) { */
/*   if(!yaml_emitter_emit(emitter, event)) */
/*     rb_raise(rb_eRuntimeError, "%s", emitter->problem); */
/* } */

/*
 * API: Ruby calls this
 *
 * Emit the start stream event to the libyaml emitter.
 * Returns 1 for success or 0 for failure.
 */
int emit_start_stream(yaml_emitter_t *emitter, yaml_encoding_t encoding) {
  fprintf(stderr, "+++ emit_start_stream(emitter, %d)\n", encoding);
  yaml_event_t event;
  yaml_stream_start_event_initialize(&event, encoding);
  return yaml_emitter_emit(emitter, &event);
}

/*
 * API: Ruby calls this
 * emitter:         The emitter to use
 * version:         NULL or pointer to two ints: [major, minor]
 * tag_directives:  Array of pairs of strings ["!", "foo:bar"]
 * implicit:        0 or 1
 *
 * Returns 1 for success or 0 for failure.
 */
int emit_start_document(yaml_emitter_t *emitter,
                        int *version,
                        unsigned char **tag_directives,
                        int num_tags,
                        int implicit) {
  int i;
  fprintf(stderr, "+++ emit_start_document\n");
  fprintf(stderr, "+++    version: [%d, %d]\n", version[0], version[1]);
  fprintf(stderr, "+++    tag_directives: num_tags: %d  implicit: %d\n", num_tags, implicit);
  fprintf(stderr, "+++    tag_directives: %p  *tag_directives: %p\n", tag_directives, *tag_directives);
  unsigned char **p = *tag_directives;
  int num_strs = num_tags * 2;
  for(i=0; i< num_strs; i++) {
    fprintf(stderr, "+++   [%d]: %p\n", i, tag_directives[i]);
  }

  yaml_version_directive_t version_directive;
  if (version != NULL) {
    version_directive.major = version[0];
    version_directive.minor = version[1];
  }

  yaml_tag_directive_t *head = NULL;
  yaml_tag_directive_t *tail = NULL;
  head = calloc(num_tags * 2, sizeof(yaml_tag_directive_t));
  tail = head;
  unsigned char **cur = tag_directives;
  for(i=0; i<num_tags; i++) {
    tail->handle = *cur++;
    tail->prefix = *cur++;
    tail++;
  }

  yaml_event_t event;
  yaml_document_start_event_initialize(
     &event,
     (version == NULL) ? NULL : &version_directive,
     head, tail, implicit);

  int result = yaml_emitter_emit(emitter, &event);
  if (head) free(head);
  return result;
}
