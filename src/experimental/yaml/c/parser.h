/*
 * Header file for the dynamic parser
 */
#ifndef __PBM_PARSER_H
#define __PBM_PARSER_H

#include <yaml.h>

typedef struct parser_context_s {
  yaml_parser_t parser;
} parser_context_t;

int parse(parser_context_t *parser_context, const unsigned char *input);

parser_context_t *parser_new();

#endif
