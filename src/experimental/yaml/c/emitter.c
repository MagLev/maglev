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
typedef struct emitter_context_s {
  yaml_emitter_t *emitter;
  unsigned char *buffer;
  size_t buffer_size;
  size_t char_count;
} emitter_context_t;

/*
 * A callback registered with libyaml so the emitter can flush output.  We don't
 * have callbacks to ruby yet, and we don't know when this will be called, so we just
 * cache output until we get back to ruby and consume it.
 */
int buffer_writer(void *ctx, unsigned char *buffer, size_t size) {
  fprintf(stderr, "+++ buffer_writer write: %ld\n", size);
  emitter_context_t *emitter_context = ctx;

  size_t bytes_left = emitter_context->buffer_size - emitter_context->char_count;
  if (bytes_left < size) {
    size_t min_size = emitter_context->buffer_size + (2 * size);
    size_t new_size = emitter_context->buffer_size * 2;
    if (min_size > new_size) {
      new_size = min_size;
    }
    fprintf(stderr, "+++ buffer_writer realloc %ld\n", new_size);
    emitter_context->buffer = realloc(emitter_context->buffer, new_size);
    emitter_context->buffer_size = new_size;
  }

  unsigned char *dest = emitter_context->buffer + emitter_context->char_count;
  memcpy(dest, buffer, size);
  emitter_context->char_count += size;
  return 1;
}

/*
 * Creates the fully initialized libyaml emitter object used by one
 * instance of Psych::Emitter.
 */
yaml_emitter_t *create_emitter() {
  yaml_emitter_t *emitter = (yaml_emitter_t *)malloc(sizeof(yaml_emitter_t));
  yaml_emitter_initialize(emitter);
  yaml_emitter_set_unicode(emitter, 1);
  return emitter;
}

/* API: Ruby calls this */
void free_emitter_context(emitter_context_t *context) {
  fprintf(stderr, "+++ free_emitter_context\n");
  if (context) {
    if (context->emitter) {
      yaml_emitter_delete(context->emitter);
    }
    if (context->buffer) {
      free(context->buffer);
    }
    free(context);
  }
}

/* API: Ruby calls this */
emitter_context_t *create_emitter_context() {
  emitter_context_t *context = (emitter_context_t *)malloc(sizeof(emitter_context_t));
  memset(context, 0, sizeof(emitter_context_t));

  context->emitter = create_emitter();
  size_t s = 1024;
  context->buffer = (unsigned char *)malloc(s);
  context->char_count = 0;
  context->buffer_size = s;

  /* RxINC: Hack until we get callbacks */
  yaml_emitter_set_output(context->emitter, buffer_writer, context);
}


char *get_error_string(yaml_emitter_t *emitter) {
  emitter->problem;
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
  fprintf(stderr, "+++    version: [%d, %d]  (ptr: %p)\n", version[0], version[1], version);
  fprintf(stderr, "+++    tag_directives: num_tags: %d  implicit: %d\n", num_tags, implicit);
  fprintf(stderr, "+++    tag_directives: %p  *tag_directives: %p\n", tag_directives, *tag_directives);
  fprintf(stderr, "+++    tag_directives: [0] %p  [1] %p\n", tag_directives[0], tag_directives[1]);

  int num_strs = num_tags * 2;
  for(i=0; i< num_strs; i++) {
    fprintf(stderr, "+++   [%d]: %p  %s\n", i, tag_directives[i], tag_directives[i]);
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

int emit_scalar(yaml_emitter_t *emitter,
                yaml_char_t *value,
                size_t value_len,
                yaml_char_t *anchor,
                yaml_char_t *tag,
                int plain,
                int quoted,
                yaml_scalar_style_t style) {

  fprintf(stderr, "+++ emit_scalar(emitter, \"%s\", %d, %s, %s, %d, %d, %d)\n",
          value, value_len,
          anchor ? anchor : "NULL",
          tag    ? tag    : "NULL",
          plain, quoted, style);

  yaml_event_t event;
  yaml_scalar_event_initialize(&event,
                               anchor,
                               tag,
                               value,
                               value_len,
                               plain,
                               quoted,
                               style);
  return yaml_emitter_emit(emitter, &event);
}

int emit_end_document(yaml_emitter_t *emitter, int implicit) {
  fprintf(stderr, "+++ emit_end_document(emitter, %d)\n", implicit);
  yaml_event_t event;
  yaml_document_end_event_initialize(&event, implicit);
  return yaml_emitter_emit(emitter, &event);
}

int emit_end_stream(yaml_emitter_t *emitter) {
  fprintf(stderr, "+++ emit_end_stream(emitter)\n");
  yaml_event_t event;
  yaml_stream_end_event_initialize(&event);
  return yaml_emitter_emit(emitter, &event);
}
