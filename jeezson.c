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
#else
#define attribute_warnunused
#endif

#if defined(__GNUC__)
#define attribute_const __attribute__((const))
#else
#define attribute_const
#endif

typedef uint32_t char32_t;

attribute_nonnull static uint8_t
utf8_cpychr(char *dest, char const *src)
{
	uint8_t len;

	/* Skip an UTF-8 character.
	 *
	 * Prefix			Number of bytes
	 * --------------------------------
	 * 0???xxxx  0 - 7	1
	 * 110?xxxx  12-13	2
	 * 1110xxxx  14-14	3
	 * 1111xxxx  15-15	4
	 * */
#define B(v, n) ((v - 1) << (n * 2))
	len = (((B(4, 15) | B(3, 14) | B(2, 13) | B(2, 12)) >>
			(((unsigned char)*src) >> 4) << 1) &
		   3);
#undef B
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
utf8_writechr(char *__restrict dest, char32_t codepoint)
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

static char const *HEX_DIGITS = "0123456789abcdef";

attribute_nonnull static uint8_t
hex16_parse(char const *__restrict src, uint16_t *__restrict pval)
{
	uint16_t val = 0;
	unsigned i;

	for (i = 0; i < 4; ++i) {
		unsigned char const c = (unsigned char)src[i];
		val = (val << 4) | ((c | 0x20) - (c <= '9' ? '0' : 'a'));
	}

	*pval = val;
	return 4;
}

attribute_nonnull static uint8_t
hex16_write(char *dest, uint16_t val)
{
	dest[0] = HEX_DIGITS[(val >> 12) & 0xf];
	dest[1] = HEX_DIGITS[(val >> 8) & 0xf];
	dest[2] = HEX_DIGITS[(val >> 4) & 0xf];
	dest[3] = HEX_DIGITS[val & 0xf];
	return 4;
}

attribute_const static char32_t
utf32_fromsurrogates(uint16_t high, uint16_t low)
{
	return 0x10000 + (((char32_t)(high & 0x3ff) << 10) | (low & 0x3ff));
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

attribute_warnunused attribute_const static int
json_iswhitespace(char c)
{
	switch (c) {
	case ' ':
	case '\t':
	case '\r':
	case '\n':
		return 1;
	}
	return 0;
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
			uint8_t nbytes = utf8_cpychr(p, s);
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
				uint16_t high, low;
				char32_t unicode;

				s += 2;
				s += hex16_parse(s, &high);

				if (high < 0xd800 || 0xdfff < high) {
					unicode = high;
				} else {
					/* Handle an UTF-16 surrogate pair. */
					s += 2;
					s += hex16_parse(s, &low);

					unicode = utf32_fromsurrogates(high, low);
				}

				p += utf8_writechr(p, unicode);
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

	goto start;
	for (;;) {
		json_node *node;

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
				continue;
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
			goto start;
			break;

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
				goto update_sibling;
			continue;

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
			continue;

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

	update_sibling:
		if (depth == 0)
			return 1;

	start:
		++nodeidx;

		if (nodeidx >= *pnnodes) {
			void *p;

			if (NULL ==
				(p = realloc(*pnodes, (*pnnodes += 32) * sizeof **pnodes)))
				return 0;

			*pnodes = p;
		}
		node = &(*pnodes)[nodeidx];
		node->keystr = NULL;
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
json_writer_term(struct json_writer *__restrict w)
{
	w->buf[w->len] = '\0';
}

void
json_writer_free(struct json_writer *__restrict w)
{
	free(w->buf);
}

#define write_lit(lit) \
	do { \
		memcpy(w->buf + w->len, lit, strlen(lit) * sizeof(char)); \
		w->len += strlen(lit); \
	} while (0)

void
json_write_null(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		write_lit(",");

	write_lit("null");
}

void
json_write_bool(struct json_writer *__restrict w, int b)
{
	if (w->open < w->len)
		write_lit(",");

	if (b)
		write_lit("true");
	else
		write_lit("false");
}

void
json_write_num(struct json_writer *__restrict w, double num)
{
	if (w->open < w->len)
		write_lit(",");

	w->len += sprintf(w->buf + w->len, "%.15g", num);
}

void
json_write_int(struct json_writer *__restrict w, unsigned long num)
{
	if (w->open < w->len)
		write_lit(",");

	w->len += sprintf(w->buf + w->len, "%lu", num);
}

void
json_write_beginarr(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		write_lit(",");

	write_lit("[");
	w->open = w->len;
}

void
json_write_endarr(struct json_writer *__restrict w)
{
	write_lit("]");
	w->open = 0;
}

void
json_write_beginobj(struct json_writer *__restrict w)
{
	if (w->open < w->len)
		write_lit(",");

	write_lit("{");
	w->open = w->len;
}

void
json_write_endobj(struct json_writer *__restrict w)
{
	write_lit("}");
	w->open = 0;
}

int
json_write_key(struct json_writer *__restrict w, char const *__restrict s)
{
	if (!json_write_str(w, s))
		return 0;
	write_lit(":");
	w->open = w->len;

	return 1;
}

int
json_write_str(struct json_writer *__restrict w, char const *__restrict s)
{
	char *p;
	char const *q;
	size_t newlen;

	if (w->open < w->len)
		write_lit(",");

	write_lit("\"");

	newlen = w->len + 1 /*apos*/;
	for (q = s; *q != '\0'; ++q) {
		if (q[0] <= 0x1f || q[0] == 0x7f /*del*/) {
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
		} else if (q[0] == '"' || q[0] == '\\') {
			newlen += 2;
		}
	}

	newlen += q - s;
	if (!ensure_size(w, newlen))
		return 0;

	p = w->buf + w->len;
	while (*s != '\0') {
		if (0x1f < s[0] && s[0] != 0x7f /*del*/) {
			if ('"' != s[0] && '\\' != s[0]) {
				uint8_t const nbytes = utf8_cpychr(p, s);
				p += nbytes, s += nbytes;
				continue;
			} else {
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
				p += hex16_write(p, s[0]);
			}
#undef B
		}

		s += 1;
	}

	w->len = p - w->buf;
	write_lit("\"");
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
		printf("%s%c%3d %s", "                                " + (32 - level),
			   json_type(node) == json_obj || json_type(node) == json_arr ? '+'
																		  : '-',
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

#undef write_lit
/* vi:set ft=c noet ts=4 sw=4: */
