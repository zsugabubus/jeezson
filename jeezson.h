/**
 * jeezson - JSON parser and generator
 * Copyright (C) 2020  zsugabubus
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef JEEZJSON_H
#define JEEZJSON_H

#include <stddef.h>

enum json_node_type {
	json_false,
	json_true,
	json_null,
	json_str,
	json_num,
	json_arr,
	json_obj
};

typedef struct json_node json_node;

struct json_node {
	size_t flags;

	enum json_node_type type;

	char const *keystr;
	union {
		char const *str;
		double num;
	} val;
};

struct json_writer {
	char *buf;
	size_t len;
	size_t size;
	size_t open;
	/* TODO(?): size_t depth. */
};

int
json_parse(char *buf, json_node *__restrict *__restrict pnodes,
		   size_t *__restrict pnnodes);
json_node *
json_get(json_node *__restrict node, char const *__restrict keystr);

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static inline size_t
json_sibl(json_node *__restrict node)
{
	return node->flags >> 3;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static inline enum json_node_type
json_type(json_node *__restrict node)
{
	return node->flags & 0x7;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static inline json_node *
json_next(json_node *__restrict node)
{
	return (json_sibl(node) > 0 ? node + json_sibl(node) : NULL);
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static inline int
json_isempty(json_node *__restrict node)
{
	return node->flags < 8;
}

int
json_writer_init(struct json_writer *__restrict w);
void
json_writer_term(struct json_writer *__restrict w);
void
json_writer_free(struct json_writer *__restrict w);

void
json_write_null(struct json_writer *__restrict w);
void
json_write_bool(struct json_writer *__restrict w, int b);
void
json_write_num(struct json_writer *__restrict w, double num);
void
json_write_int(struct json_writer *__restrict w, unsigned long num);
void
json_write_beginarr(struct json_writer *__restrict w);
void
json_write_endarr(struct json_writer *__restrict w);
void
json_write_beginobj(struct json_writer *__restrict w);
void
json_write_endobj(struct json_writer *__restrict w);
int
json_write_str(struct json_writer *__restrict w, char const *__restrict s);
int
json_write_key(struct json_writer *__restrict w, char const *__restrict s);

#endif
/* vi:set ft=c noet ts=4 sw=4: */
