
yaml_emitter_t *create_emitter() {
  yaml_emitter_t * emitter = (yaml_emitter_t *)malloc(sizeof(yaml_emitter_t));
  yaml_emitter_initialize(emitter);
  yaml_emitter_set_unicode(emitter, 1);
}
