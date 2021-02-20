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
#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <locale.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "jeezson.h"

#if defined(__GNUC__) || defined(__clang__)
#define attribute_nonnull __attribute__((nonnull))
#define attribute_returnsnonnull __attribute__((returns_nonnull))
#else
#define attribute_nonnull
#define attribute_returnsnonnull
#endif

#if defined(__GNUC__)
#define attribute_alwaysinline __attribute__((always_inline))
#define attribute_const __attribute__((const))
#else
#define attribute_alwaysinline
#define attribute_const
#endif

typedef uint32_t char32_t;
typedef uint16_t char16_t;

attribute_alwaysinline
static __inline__ uint8_t
utf8_chrlen(char s)
{
	/*
	 * Prefix               Number of bytes
	 * --------------------------------
	 * 0???xxxx  0 - 7	1
	 * 110?xxxx  12-13	2
	 * 1110xxxx  14-14	3
	 * 1111xxxx  15-15	4
	 *
	 * */
#define B(v, n) ((v - 1) << (n * 2))
	return (((uint32_t)(B(4, 15) | B(3, 14) | B(2, 13) | B(2, 12)) >>
			((((unsigned char)s) >> 4) << 1)) &
		   3);
#undef B
}

attribute_nonnull static __inline__ uint8_t
utf8_chrcpy(char *dest, char const *src)
{
	uint8_t const len = utf8_chrlen(*src);

	/* TODO: Measure against `rep mov'. */
	switch (len) {
	case 3:
		dest[len - 3] = src[len - 3]; /* fall through */
	case 2:
		dest[len - 2] = src[len - 2]; /* fall through */
	case 1:
		dest[len - 1] = src[len - 1]; /* fall through */
	default:
		dest[len] = src[len];
	}
	return len + 1;
}

attribute_nonnull static uint8_t
utf32_toutf8(char *__restrict dest, char32_t codepoint)
{
	if (codepoint <= 0x7f) {
		/* U+0000..U+007f */
		dest[0] = codepoint;
		return 1;
	} else if (codepoint <= 0x7ff) {
		/* U+0080..U+07FF */
		dest[0] = 0xc0 | codepoint >> 6;
		dest[1] = 0x80 | (codepoint & 0x3f);
		return 2;
	} else if (codepoint <= 0xffff) {
		/* U+0800..U+FFFF */
		dest[0] = 0xe0 | codepoint >> 12;
		dest[1] = 0x80 | (codepoint >> 6 & 0x3f);
		dest[2] = 0x80 | (codepoint & 0x3f);
		return 3;
	} else {
		/* U+10000..U+10ffff */
		dest[0] = 0xf0 | codepoint >> 18;
		dest[1] = 0x80 | (codepoint >> 12 & 0x3f);
		dest[2] = 0x80 | (codepoint >> 6 & 0x3f);
		dest[3] = 0x80 | (codepoint & 0x3f);
		return 4;
	}
}

attribute_const static char32_t
utf32_fromsurrogates(char16_t high, char16_t low)
{
	return 0x10000U + (((char32_t)(high & 0x03ffU) << 10) | (low & 0x03ffU));
}

attribute_const attribute_alwaysinline
static __inline__ int
ascii_iscntrl(char c)
{
	return (unsigned char)c <= 0x1fU || (unsigned char)c == 0x7fU/*del*/;
}

attribute_const attribute_alwaysinline
static __inline__ int
json_iswhitespace(char c)
{
	return ' ' == c || '\t' == c || '\r' == c || '\n' == c;
}

attribute_const attribute_alwaysinline
static __inline__ int
json_isspecial(char c)
{
	return '"' == c || '\\' == c;
}

static uint8_t const HEX_LOOKUP[16 + 10] = {
	0, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8, 0x9
};

static char const *const HEX_DIGITS = "0123456789abcdef";

attribute_nonnull attribute_alwaysinline
static __inline__ uint8_t
hex8_fromstr(char const *__restrict src)
{
	return
		((HEX_LOOKUP[src[0] % 32]) << 4) |
		((HEX_LOOKUP[src[1] % 32]) << 0);
}

attribute_nonnull attribute_alwaysinline
static __inline__ uint16_t
hex16_fromstr(char const *__restrict src)
{
	return
		((HEX_LOOKUP[src[0] % 32]) << 12) |
		((HEX_LOOKUP[src[1] % 32]) << 8) |
		((HEX_LOOKUP[src[2] % 32]) << 4) |
		((HEX_LOOKUP[src[3] % 32]) << 0);
}

attribute_nonnull attribute_alwaysinline
static __inline__ void
hex16_tostr(char *dest, uint16_t val)
{
	dest[0] = HEX_DIGITS[(val >> 12) % 16];
	dest[1] = HEX_DIGITS[(val >>  8) % 16];
	dest[2] = HEX_DIGITS[(val >>  4) % 16];
	dest[3] = HEX_DIGITS[ val        % 16];
}

static int
ensure_size(struct json_writer *__restrict w, size_t size)
{
	char *p;

	size += 32 /*deepest depth*/ + 32 /*longest token:number*/ +
	        1 /*extra colon*/ + 1 /*nil*/;
	if (size <= w->size)
		return 1;

	if (NULL == (p = realloc(w->buf, (w->size = size))))
		return 0;
	w->buf = p;

	return 1;
}

attribute_const attribute_nonnull struct json_node *
json_get(struct json_node const *__restrict node, char const *__restrict key)
{
	size_t keysize;

	assert(json_obj == json_type(node));
	if (0 == node->val.len)
		return NULL;

	keysize = strlen(key) + 1;

	node = json_children(node);
	do {
		if (0 == memcmp(node->key, key, keysize))
			break;
	} while (NULL != (node = json_next(node)));

	return (struct json_node *)node;
}

attribute_nonnull attribute_returnsnonnull static char *
parse_str(char *__restrict s)
{
	char *p;

	for (p = ++s; '"' != s[0];) {
		if (s[0] != '\\') {
			uint8_t const nbytes = utf8_chrcpy(p, s);
			p += nbytes, s += nbytes;
		} else {
			switch (s[1]) {
			default:
				p[0] = s[1];
				break;

			case 'b':
				p[0] = '\b';
				break;

			case 'f':
				p[0] = '\f';
				break;

			case 'n':
				p[0] = '\n';
				break;

			case 'r':
				p[0] = '\r';
				break;

			case 't':
				p[0] = '\t';
				break;

			case 'x':
			{
				s += 2;
				*p = hex8_fromstr(s);
				p += 1, s += 2;
			}
				continue;

			case 'u':
			{
				char16_t high, low;
				char32_t unicode;

				s += 2;
				high = hex16_fromstr(s);
				s += 4;

				if (high < 0xd800 || 0xdfff < high) {
					unicode = high;
				} else {
					/* UTF-16 surrogate pair. */
					s += 2;
					low = hex16_fromstr(s);
					s += 4;

					unicode = utf32_fromsurrogates(high, low);
				}

				p += utf32_toutf8(p, unicode);
			}
				continue;
			}
			p += 1, s += 2;
		}
	}
	/* zero-terminate output */
	p[0] = '\0';
	return s + 1/* final ‘"’ */;
}

attribute_nonnull int
json_parse(char *s, struct json_node *__restrict *__restrict pnodes, size_t *__restrict pnnodes)
{
	uint8_t depth = 0;
	size_t nodeidx = -1;
	size_t isobj = 0;
	size_t parents[sizeof isobj * CHAR_BIT];
	size_t lengths[sizeof isobj * CHAR_BIT];
	size_t siblings[sizeof isobj * CHAR_BIT];
	locale_t origloc = (locale_t)0;
	static locale_t cloc = (locale_t)0;

	if ((locale_t)0 == cloc)
		cloc = newlocale(LC_NUMERIC_MASK, "C", (locale_t)0);

	for (;;) {
		struct json_node *node;

		++nodeidx;

		if (*pnnodes <= nodeidx) {
			void *p;
			/* increment size by golden ratio (~1.6) */
			size_t nnodes = (*pnnodes * 8 / 5) + 1;

			if (NULL == (p = realloc(*pnodes, nnodes * sizeof **pnodes))) {
				uselocale(origloc);
				return 0;
			}
			*pnodes = p;
			*pnnodes = nnodes;
		}

		node = &(*pnodes)[nodeidx];
		node->key = NULL;

	parse_token:
		while (json_iswhitespace(*s))
			++s;

		switch (*s) {
		case '"': {
			int const isval = !(isobj & ((size_t)1 << depth)) || NULL != node->key;
			if (isval) {
				node->sibltype = json_str;
				node->val.str = s + 1;
			} else {
				node->key = s + 1;
			}

			s = parse_str(s);

			/* >> (depth + 1); underflowed? */
			if (isval) {
				break;
			} else {
				while (*s++ != ':')
					;
				goto parse_token;
			}
		}

		case '[':
			node->sibltype = json_arr;
			goto init_container;
		case '{':
			node->sibltype = json_obj;
			isobj ^= (2 << depth);
		init_container:
			++depth;
			assert(depth < sizeof isobj * CHAR_BIT);
			lengths[depth] = 0;
			parents[depth] = nodeidx;
			siblings[depth] = nodeidx + 1;
			s += 1;
			continue;

		case '}':
			isobj ^= (1 << depth);
			/* fall through */
		case ']':
			(*pnodes)[parents[depth]].val.len = lengths[depth] + !!(parents[depth] != nodeidx - 1);
			s += 1;
			if (--depth == 0)
				break;
			goto parse_token;

		case 'f':
			node->sibltype = json_false;
			s += strlen("false");
			break;

		case 't':
			node->sibltype = json_true;
			s += strlen("true");
			break;

		case ',':
			++lengths[depth];

			(*pnodes)[siblings[depth]].sibltype |= (nodeidx - siblings[depth]) << 3;
			siblings[depth] = nodeidx;

			s += 1;
			goto parse_token;

		case 'n':
			node->sibltype = json_null;
			s += strlen("null");
			break;

		default:
			if ((locale_t)0 == origloc)
				origloc = uselocale(cloc);
			node->sibltype = json_num;
			node->val.num = strtod(s, &s);
			break;
		}

		if (depth == 0)
			break;
	}

	(void)uselocale(origloc);
	return 1;
}

static int
ensure_size(struct json_writer *__restrict w, size_t newlen);

int
json_writer_init(struct json_writer *__restrict w)
{
	w->buf = NULL;
	w->len = 0;
	w->size = 0;
	w->open = 0;
	return ensure_size(w, 0);
}

int
json_write_str(struct json_writer *__restrict w, char const *__restrict s)
{
	char *p;
	char const *q;
	size_t newlen;

	/* count how much bytes escaping will consume */
	newlen = w->len + 1/*[comma]*/ + 1/*apos*/ + 1/*apos*/;
	for (q = s; *q != '\0'; ++q) {
		if (ascii_iscntrl(q[0])) {
			switch (q[0]) {
			case '\b':
			case '\t':
			case '\n':
			case '\f':
			case '\r':
				newlen += 2;
				break;
			default:
				newlen += 6;
				break;
			}
		} else if (json_isspecial(q[0])) {
			newlen += 2;
		}
	}

	newlen += q - s;
	if (!ensure_size(w, newlen))
		return 0;

	p = w->buf + w->len;

	if (w->open < w->len)
		*p++ = ',';

	*p++ = '\"';
	while (*s != '\0') {
		if (!ascii_iscntrl(s[0])) {
			if (!json_isspecial(s[0])) {
				uint8_t const nbytes = utf8_chrcpy(p, s);
				p += nbytes, s += nbytes;
				continue;
			} else {
				/* extra escaping is needed */
				p[0] = '\\';
				p[1] = s[0];
				p += 2;
			}
		} else {
			p[0] = '\\';
#define B(c) (1 << c)
			if ((B('\b') | B('\t') | B('\n') | B('\f') | B('\r')) &
				(1 << s[0])) {
				p[1] = "btn:)fr"[s[0] - '\b'];
				p += 2;
			} else {
				p[1] = 'u';
				p += 2;
				hex16_tostr(p, s[0]);
				p += 4;
			}
#undef B
		}

		s += 1;
	}

	*p++ = '\"';
	w->len = p - w->buf;
	return 1;
}

int
json_write_val(struct json_writer *__restrict w, struct json_node const *node) {
	enum json_node_type type = json_type(node);

	/* TODO: Get rid of recursion. */
	switch (type) {
	case json_str:
		return json_write_str(w, node->val.str);

	case json_false:
	case json_true:
		return json_write_bool(w, type == json_true), 1;

	case json_null:
		return json_write_null(w), 1;

	case json_num:
		return json_write_num(w, node->val.num), 1;

	case json_arr:
		json_write_beginarr(w);
		if (!json_isempty(node)) {
			++node;
			do {
				if (!json_write_val(w, node))
					return 0;
			} while (NULL != (node = json_next(node)));
		}
		json_write_endarr(w);
		return 1;

	case json_obj:
		json_write_beginobj(w);
		if (!json_isempty(node)) {
			++node;
			do {
				if (!json_write_key(w, node->key))
					return 0;
				if (!json_write_val(w, node))
					return 0;
			} while (NULL != (node = json_next(node)));
		}
		json_write_endobj(w);
		return 1;
	}

#if defined(__GNUC__) || defined(__clang__)
	__builtin_unreachable();
#else
	return 0;
#endif
}

static void
json_dump_internal(struct json_node const *node, unsigned level, FILE *stream)
{
	enum json_node_type type;

	if (NULL == node) {
		fprintf(stream, "(null)\n");
		return;
	}

	type = json_type(node);

	fprintf(stream, "%*s", level, "");

	if (node->key)
		fprintf(stream, "%s=", node->key);

	switch (type) {
	case json_obj:
	case json_arr:
		fprintf(stream, "%c\n", json_obj == type ? '{' : '[');
		if (!json_isempty(node)) {
			node = json_children(node);
			do
				json_dump_internal(node, level + 1, stream);
			while ((node = json_next(node)));
		}
		fprintf(stream, "%*s%c\n", level, "", json_obj == type ? '}' : ']');
		break;

	case json_num:
	{
		int64_t const int_num = node->val.num;
		if (node->val.num == int_num)
			fprintf(stream, "%"PRId64"\n", int_num);
		else
			fprintf(stream, "%g\n", node->val.num);
	}
		break;

	case json_str:
		fprintf(stream, "\"%s\"\n", node->val.str);
		break;

	case json_true:
	case json_false:
		fprintf(stream, "%s\n", json_true == type ? "true" : "false");
		break;

	case json_null:
		fprintf(stream, "null\n");
		break;

	default:
		fprintf(stream, "(bug)\n");
		break;
	}
}

void
json_dump(struct json_node const *node, FILE *stream)
{
	json_dump_internal(node, 0, stream);
}
