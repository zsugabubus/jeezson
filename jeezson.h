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
	size_t sibltype;

	char const *keystr;
	union {
		char const *str;
		double num;
		size_t len;
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
	return node->sibltype >> 3;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static inline enum json_node_type
json_type(json_node *__restrict node)
{
	return node->sibltype & 0x7;
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
	assert(json_type(node) == json_obj || json_type(node) == json_arr);
	return node->val.len;
}

int
json_writer_init(struct json_writer *__restrict w);

static inline void
json_writer_term(struct json_writer *__restrict w)
{
	w->buf[w->len] = '\0';
}

void
json_writer_free(struct json_writer *__restrict w);


#define json_write_lit(lit) \
	do { \
		memcpy(w->buf + w->len, lit, strlen(lit) * sizeof(char)); \
		w->len += strlen(lit); \
	} while (0)

static inline void
json_write_null(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("null");
}

static inline void
json_write_bool(struct json_writer *__restrict w, int b)
{
	if (w->open < w->len)
		json_write_lit(",");

	if (b)
		json_write_lit("true");
	else
		json_write_lit("false");
}

static inline void
json_write_num(struct json_writer *__restrict w, double num)
{
	if (w->open < w->len)
		json_write_lit(",");

	w->len += sprintf(w->buf + w->len, "%.15g", num);
}

static inline void
json_write_int(struct json_writer *__restrict w, unsigned long num)
{
	if (w->open < w->len)
		json_write_lit(",");

	w->len += sprintf(w->buf + w->len, "%lu", num);
}

static inline void
json_write_beginarr(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("[");
	w->open = w->len;
}

static inline void
json_write_endarr(struct json_writer *__restrict w)
{
	json_write_lit("]");
	w->open = 0;
}

static inline void
json_write_beginobj(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("{");
	w->open = w->len;
}

static inline void
json_write_endobj(struct json_writer *__restrict w)
{
	json_write_lit("}");
	w->open = 0;
}

int
json_write_str(struct json_writer *__restrict w, char const *__restrict s);

static inline int
json_write_key(struct json_writer *__restrict w, char const *__restrict s)
{
	if (!json_write_str(w, s))
		return 0;
	json_write_lit(":");
	w->open = w->len;

	return 1;
}

#undef json_write_lit

void
json_debug(json_node *node, unsigned level);

#endif
/* vi:set ft=c noet ts=4 sw=4: */
