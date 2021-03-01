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
#ifndef JEEZSON_H
#define JEEZSON_H

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern unsigned json_dump_max_level;
extern unsigned json_flags;

#define JSON_FLAG_SKIP_NUMBERS 1

#if __STDC_VERSION__ >= 199901L
# define json_each(child, node) \
	for (JSONNode *child = json_first(node); \
	     child; \
	     child = json_next(child))
#else
# define json_each(child, node) \
	for (child = json_first(node); \
	     child; \
	     child = json_next(child))
#endif

enum JSONNodeType {
	JSONT_FALSE,
	JSONT_TRUE,
	JSONT_NULL,
	JSONT_STRING,
	JSONT_NUMBER,
	JSONT_ARRAY,
	JSONT_OBJECT
};

typedef struct JSONNode {
	size_t sibltype;

	char const *key;
	union {
		char const *str; /* If JSONT_STRING. */
		double num; /* If JSONT_NUMBER. */
		size_t len; /* If JSONT_ARRAY || JSONT_OBJECT. */
	}
#if __STDC_VERSION__ < 201112L
		u
#endif
	;
} JSONNode;

typedef struct JSONWriter {
	char *buf;
	size_t size;
	size_t alloced;
	size_t open;
	int status;
} JSONWriter;

/**
 * @return number of parsed bytes
 */
size_t json_parse(char *buf, JSONNode *__restrict *__restrict pnodes, size_t *__restrict pnb_nodes);

#if defined(__GNUC__)
__attribute__((const))
#endif
JSONNode *json_get(JSONNode const *__restrict node, char const *__restrict keystr);

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ enum JSONNodeType
json_type(JSONNode const *__restrict node)
{
	return node->sibltype & 0x7;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ JSONNode *
json_next(JSONNode const *__restrict node)
{
	size_t next_sibling = node->sibltype >> 3;
	return (JSONNode *)(0 < next_sibling ? node + next_sibling : NULL);
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ size_t
json_length(JSONNode const *__restrict node)
{
	assert(JSONT_OBJECT == json_type(node) || JSONT_ARRAY == json_type(node));
	return node->
#if __STDC_VERSION__ < 201112L
		u.
#endif
		len;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ JSONNode *
json_children(JSONNode const *__restrict node)
{
	assert(json_length(node));
	return (JSONNode *)(node + 1);
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ JSONNode *
json_first(JSONNode const *__restrict node)
{
	return (JSONNode *)(node && json_length(node) ? json_children(node) : NULL);
}

void json_writer_init(JSONWriter *__restrict w);

static __inline__ void
json_writer_reset(JSONWriter *__restrict w)
{
	w->status = 0;
	w->size = 0;
	w->open = 0;
}

static __inline__ void
json_writer_free(JSONWriter *__restrict w)
{
	free(w->buf);
}

void json_write_null(JSONWriter *__restrict w);
void json_write_bool(JSONWriter *__restrict w, int b);
void json_write_num(JSONWriter *__restrict w, double num);
void json_write_int(JSONWriter *__restrict w, long num);
void json_write_str(JSONWriter *__restrict w, char const *__restrict s);
void json_write_value(JSONWriter *__restrict w, JSONNode const *value);
void json_write_beginarr(JSONWriter *__restrict w);
void json_write_endarr(JSONWriter *__restrict w);
void json_write_beginobj(JSONWriter *__restrict w);
void json_write_endobj(JSONWriter *__restrict w);
void json_write_key(JSONWriter *__restrict w, char const *__restrict s);
#define json_write(w, x) _Generic(x, \
	bool: json_write_bool, \
	signed char: json_write_int, \
	signed short: json_write_int, \
	signed int: json_write_int, \
	signed long: json_write_int, \
	unsigned char: json_write_int, \
	unsigned short: json_write_int, \
	unsigned int: json_write_int, \
	unsigned long: json_write_int, \
	float: json_write_num, \
	double: json_write_num, \
	long double: json_write_num, \
	char *: json_write_str \
)(w, x)
#define json_write_entry(w, key, value) (json_write_key(w, key), json_write(w, value))

char *json_get_string(JSONNode const *node);

void json_dump(JSONNode const *node, FILE *stream);

#endif
