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
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include <endian.h> */

#include "jeezson.h"

#if defined(__GNUC__) || defined(__clang__)
#define attribute_nonnull __attribute__((nonnull))
#define attribute_returnsnonnull __attribute__((returns_nonnull))
#else
#define attribute_nonnull
#define attribute_returnsnonnull
#endif

#if defined(__GNUC__)
#define attribute_warnunused __attribute__((warn_unused_result))
#define attribute_alwaysinline __attribute__((always_inline))
#else
#define attribute_warnunused
#define attribute_alwaysinline
#endif

#if defined(__GNUC__)
#define attribute_const __attribute__((const))
#else
#define attribute_const
#endif

typedef uint32_t char32_t;
typedef uint16_t char16_t;

attribute_alwaysinline
static __inline__ uint8_t
utf8_chrlen(char s)
{
	/*
	 * Prefix			Number of bytes
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

	switch (len) {
	case 3:
		dest[len - 3] = src[len - 3];
		/* Fall through. */
	case 2:
		dest[len - 2] = src[len - 2];
		/* Fall through. */
	case 1:
		dest[len - 1] = src[len - 1];
		/* Fall through. */
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
	return 0x10000U + (((char32_t)(high & 0x3ffU) << 10) | (low & 0x3ffU));
}

attribute_warnunused attribute_const attribute_alwaysinline
static __inline__ int
ascii_iscntrl(char c)
{
	return (unsigned char)c <= 0x1fU || (unsigned char)c == 0x7fU/*del*/;
}

attribute_warnunused attribute_const attribute_alwaysinline
static __inline__ int
json_iswhitespace(char c)
{
	return ' ' == c || '\t' == c || '\r' == c || '\n' == c;
}

attribute_warnunused attribute_const attribute_alwaysinline
static __inline__ int
json_isspecial(char c)
{
	return '"' == c || '\\' == c;
}

attribute_nonnull attribute_alwaysinline
static __inline__ uint16_t
hex16_fromstr(char const *__restrict src)
{
	union {
		uint8_t a[4];
		uint32_t v;
	} u;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
	u.a[3] = src[0];
	u.a[1] = src[1];
	u.a[2] = src[2];
	u.a[0] = src[3];
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#warning "Untested endianness."
	u.a[0] = src[0];
	u.a[2] = src[1];
	u.a[1] = src[2];
	u.a[3] = src[3];
#elif __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
#warning "Untested endianness."
	u.a[1] = src[0];
	u.a[3] = src[1];
	u.a[0] = src[2];
	u.a[2] = src[3];
#else
#error "Unknown endianness."
#endif

/*
ch dec	bin
0  48	00110000
9  57	00111001
  xor	01100111 0x67
a-10	01010111
a  97	01100001
f  102	01100110
*/
#define S(v) (((v) << 24) | ((v) << 16) | ((v) << 8) | ((v) << 0))
	/* Make input lowercase. */
	u.v |= S(0x20U);
	u.v -= S('a' - 10U) ^ ((u.v & S(0x10U)) >> 4) * 0x67U;
#undef S

	return (uint16_t)((u.v >> 12) | u.v);
}

static char const *HEX_DIGITS = "0123456789abcdef";

attribute_nonnull attribute_alwaysinline
static __inline__ void
hex16_tostr(char *dest, uint16_t val)
{
	dest[0] = HEX_DIGITS[(val >> 12) & 0xf];
	dest[1] = HEX_DIGITS[(val >> 8) & 0xf];
	dest[2] = HEX_DIGITS[(val >> 4) & 0xf];
	dest[3] = HEX_DIGITS[val & 0xf];
}

attribute_warnunused static int
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

attribute_warnunused attribute_nonnull json_node *
json_get(json_node *__restrict node, char const *__restrict keystr)
{
	size_t keysize;

	if (0 == node->val.len)
		return NULL;
	/* if (json_isempty(node))
		return NULL; */

	keysize = strlen(keystr) + 1;

	++node;
	do {
		assert(node->keystr || !"BUG: Key is null");
		if (0 == memcmp(node->keystr, keystr, keysize))
			break;
	} while (NULL != (node = json_next(node)));

	return node;
}

attribute_warnunused attribute_nonnull attribute_returnsnonnull static char *
parse_str(char *__restrict s)
{
	char *p;

	for (p = ++s; s[0] != '"';) {
		if (s[0] != '\\') {
			uint8_t nbytes = utf8_chrcpy(p, s);
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
			case 'u': {
				char16_t high, low;
				char32_t unicode;

				s += 2;
				high = hex16_fromstr(s);
				s += 4;

				if (high < 0xd800 || 0xdfff < high) {
					unicode = high;
				} else {
					/* Handle an UTF-16 surrogate pair. */
					s += 2;
					low = hex16_fromstr(s);
					s += 4;

					unicode = utf32_fromsurrogates(high, low);
				}

				p += utf32_toutf8(p, unicode);
				continue;
			}
			}
			p += 1, s += 2;
		}
	}
	/* Overwrite final `"'. */
	*s++ = '\0';
	return s;
}

attribute_nonnull int
json_parse(char *s, json_node *__restrict *__restrict pnodes,
		   size_t *__restrict pnnodes)
{
	uint8_t depth = 0;
	size_t nodeidx = -1;
	size_t isobj = 0;
	size_t parents[sizeof isobj * CHAR_BIT];
	size_t lengths[sizeof isobj * CHAR_BIT];
	size_t siblings[sizeof isobj * CHAR_BIT];

	for (;;) {
		json_node *node;

		++nodeidx;

		/* TODO: Allocate more professionally. */
		if (nodeidx >= *pnnodes) {
			void *p;

			/* FIXME: Increment only if allocation was successful. */
			if (NULL ==
				(p = realloc(*pnodes, (*pnnodes += 32) * sizeof **pnodes)))
				return 0;

			*pnodes = p;
		}

		node = &(*pnodes)[nodeidx];
		node->keystr = NULL;

	parse_token:
		while (json_iswhitespace(*s))
			++s;

		printf("#(%d/%d)%s\n", depth, (int)nodeidx, "");
		switch (*s) {
		case '"': {
			int const isval = !(isobj & (1 << depth)) || NULL != node->keystr;
			node->sibltype = json_str;
			if (isval)
				node->val.str = s + 1;
			else
				node->keystr = s + 1;

			s = parse_str(s);

			/* >> (depth + 1); underflowed? */
			if (isval) {
				printf(" --val: %s\n", node->val.str);
				break;
			} else {
				printf(" --key: %s\n", node->keystr);
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
			assert(depth < 32);
			lengths[depth] = 0;
			parents[depth] = nodeidx;
			siblings[depth] = nodeidx + 1;
			printf(">>> parents[%d] = %d\n", depth, (int)nodeidx);
			s += 1;
			continue;

		case '}':
			isobj ^= (1 << depth);
			/* Fall through. */
		case ']':
			printf("<<< parents[%d] = %d; ch=%d\n", depth, (int)parents[depth],
				   (int)lengths[depth]);
			(*pnodes)[parents[depth]].val.len =
				lengths[depth] + !!(parents[depth] != nodeidx);
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

			printf("on depth %d %d->%d\n", depth, (int)siblings[depth],
				   (int)nodeidx);

			(*pnodes)[siblings[depth]].sibltype |= (nodeidx - siblings[depth])
												   << 3;
			siblings[depth] = nodeidx;

			s += 1;
			goto parse_token;

		case 'n':
			node->sibltype = json_null;
			s += strlen("null");
			break;

		default:
			printf("--(num)\n");
			node->sibltype = json_num;
			node->val.num = strtod(s, &s);
			break;
		}

		if (depth == 0)
			return 1;
	}

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

void
json_writer_free(struct json_writer *__restrict w)
{
	free(w->buf);
}

int
json_write_str(struct json_writer *__restrict w, char const *__restrict s)
{
	char *p;
	char const *q;
	size_t newlen;

	/* Count how much bytes escaping will consume. */
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
			printf("k %x\n", s[0]);
		if (!ascii_iscntrl(s[0])) {
			printf("c %x\n", *s);
			if (!json_isspecial(s[0])) {
				uint8_t const nbytes = utf8_chrcpy(p, s);
				printf("kkk %d\n", nbytes);
				p += nbytes, s += nbytes;
				continue;
			} else {
				/* Extra escaping is needed. */
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

void
json_debug(json_node *node, unsigned level)
{
	if (NULL == node) {
		printf("(null)\n");
		return;
	}

	do {
		printf("%s%c%3d %s", "								  " + (32 - level),
			   "-+"[json_type(node) == json_obj || json_type(node) == json_arr],
			   (unsigned)node->sibltype >> 3, node->keystr ? node->keystr : "");
		switch (json_type(node)) {
		case json_obj:
		case json_arr:
			printf(":\n");
			if (node->val.len > 0) {
				json_debug(node + 1, level + 1);
			}
			break;
		case json_num:
			printf(":(num)%f\n", node->val.num);
			break;
		case json_str:
			printf(":(str)%s\n", node->val.str);
			break;
		default:
			printf("\n");
		}
	} while ((node = json_next(node)));
}
/* vi:set ft=c noet ts=4 sw=4: */
