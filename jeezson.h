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
#include <stdlib.h>
#include <string.h>

enum json_node_type {
	json_false,
	json_true,
	json_null,
	json_str,
	json_num,
	json_arr,
	json_obj
};

struct json_node {
	size_t sibltype;

	char *key;
	union {
		char *str;
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
json_parse(char *buf, struct json_node *__restrict *__restrict pnodes,
		   size_t *__restrict pnnodes);

#if defined(__GNUC__)
__attribute__((const))
#endif
struct json_node *
json_get(struct json_node const *__restrict node, char const *__restrict keystr);

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ size_t
json_sibl(struct json_node const *__restrict node)
{
	return node->sibltype >> 3;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ enum json_node_type
json_type(struct json_node const *__restrict node)
{
	return node->sibltype & 0x7;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ struct json_node *
json_next(struct json_node const *__restrict node)
{
	return (struct json_node *)(json_sibl(node) > 0 ? node + json_sibl(node) : NULL);
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ size_t
json_len(struct json_node const *__restrict node)
{
	assert(json_obj == json_type(node) || json_arr == json_type(node));
	return node->val.len;
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ int
json_isempty(struct json_node const *__restrict node)
{
	return 0 == json_len(node);
}

/* TODO: Maybe rename. */
#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ struct json_node *
json_children(struct json_node const *__restrict node)
{
	assert(!json_isempty(node));
	return (struct json_node *)(node + 1);
}

#if defined(__GNUC__)
__attribute__((const, always_inline))
#endif
static __inline__ struct json_node *
json_first(struct json_node const *__restrict node)
{
	return (struct json_node *)(node && !json_isempty(node) ? json_children(node) : NULL);
}

int
json_writer_init(struct json_writer *__restrict w);

static __inline__ void
json_writer_term(struct json_writer *__restrict w)
{
	w->buf[w->len] = '\0';
}

static __inline__ void
json_writer_empty(struct json_writer *__restrict w)
{
	w->len = 0;
	w->open = 0;
}

static __inline__ void
json_writer_uninit(struct json_writer *__restrict w)
{
	free(w->buf);
}

#define json_write_lit(lit) \
	do { \
		memcpy(w->buf + w->len, lit, strlen(lit) * sizeof(char)); \
		w->len += strlen(lit); \
	} while (0)

static __inline__ void
json_write_null(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("null");
}

static __inline__ void
json_write_bool(struct json_writer *__restrict w, int b)
{
	if (w->open < w->len)
		json_write_lit(",");

	if (b)
		json_write_lit("true");
	else
		json_write_lit("false");
}

static __inline__ void
json_write_num(struct json_writer *__restrict w, double num)
{
	if (w->open < w->len)
		json_write_lit(",");

	w->len += sprintf(w->buf + w->len, "%.15g", num);
}

static __inline__ void
json_write_int(struct json_writer *__restrict w, long num)
{
	if (w->open < w->len)
		json_write_lit(",");

	w->len += sprintf(w->buf + w->len, "%ld", num);
}

static __inline__ void
json_write_beginarr(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("[");
	w->open = w->len;
}

static __inline__ void
json_write_endarr(struct json_writer *__restrict w)
{
	json_write_lit("]");
	w->open = 0;
}

static __inline__ void
json_write_beginobj(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		json_write_lit(",");

	json_write_lit("{");
	w->open = w->len;
}

static __inline__ void
json_write_endobj(struct json_writer *__restrict w)
{
	json_write_lit("}");
	w->open = 0;
}

int
json_write_str(struct json_writer *__restrict w, char const *__restrict s);

static __inline__ int
json_write_key(struct json_writer *__restrict w, char const *__restrict s)
{
	if (!json_write_str(w, s))
		return 0;
	json_write_lit(":");
	w->open = w->len;

	return 1;
}

int
json_write_val(struct json_writer *__restrict w, struct json_node const *value);

static __inline__ char *
json_tostring(struct json_node const *node)
{
	struct json_writer w[1];

	if (!json_writer_init(w))
		return NULL;

	if (!json_write_val(w, node)) {
		json_writer_uninit(w);
		return NULL;
	}

	json_writer_term(w);

	return w->buf;
}

#undef json_write_lit

void
json_dump(struct json_node const *node, FILE *stream);

#endif
