#include <yaml.h>
yaml_emitter_t *create_emitter() {
  yaml_emitter_t * emitter = (yaml_emitter_t *)malloc(sizeof(yaml_emitter_t));
  yaml_emitter_initialize(emitter);
  yaml_emitter_set_unicode(emitter, 1);
  return emitter;
}

/* void emit(yaml_emitter_t *emitter, yaml_event_t *event) { */
/*   if(!yaml_emitter_emit(emitter, event)) */
/*     rb_raise(rb_eRuntimeError, "%s", emitter->problem); */
/* } */

void start_stream(yaml_emitter_t *emitter, yaml_encoding_t encoding) {
  yaml_event_t event;
  yaml_stream_start_event_initialize(&event, encoding);
  return yaml_emitter_emit(emitter, &event);
}
