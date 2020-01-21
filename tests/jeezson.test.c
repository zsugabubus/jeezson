#include <test.h>

#include "jeezson.h"
#include "jeezson.c"

SUITE_FILE

TEST(utf8_chrlen) {
	expect_equal(utf8_chrlen('a'), 0);
	expect_equal(utf8_chrlen('\x7f'), 0);
	expect_equal(utf8_chrlen('\xc2'), 1);
	expect_equal(utf8_chrlen('\xd5'), 1);
	expect_equal(utf8_chrlen('\xea'), 2);
	expect_equal(utf8_chrlen('\xe2'), 2);
	expect_equal(utf8_chrlen('\xed'), 2);
	expect_equal(utf8_chrlen('\xfc'), 3);
}

TEST(utf8_chrcpy) {
	char buf[10];
	unsigned i;

#define utf8_chrcpy_test(lit) do { \
	for (i = 0; i <= sizeof lit; ++i) { \
		memset(buf, 0xa5, sizeof buf); \
		memcpy(buf + i, lit, sizeof lit); \
		expect_equal(sizeof lit - 1, utf8_chrcpy(buf, lit)); \
		buf[sizeof lit - 1] = '\0'; \
		expect_equal(&buf, lit); \
	} \
} while (0)

	utf8_chrcpy_test("\x24");
	utf8_chrcpy_test("\xc2\xa2");

	utf8_chrcpy_test("\xe0\xa4\xb9");
	utf8_chrcpy_test("\xe2\x82\xac");
	utf8_chrcpy_test("\xed\x95\x9c");

	utf8_chrcpy_test("\xf0\x90\x8d\x88");

#undef utf8_chrcpy_test
}

TEST(utf32_toutf8) {
	char dest[5];

#define utf32_toutf8_test(cp, utf8str) do { \
	dest[utf32_toutf8(dest, cp)] = '\0'; \
	expect_equal(&dest, utf8str); \
} while (0)

	utf32_toutf8_test(0x24, "\x24");

	utf32_toutf8_test(0xa2, "\xc2\xa2");

	utf32_toutf8_test(0x0939, "\xe0\xa4\xb9");
	utf32_toutf8_test(0x20ac, "\xe2\x82\xac");
	utf32_toutf8_test(0xd55c, "\xed\x95\x9c");

	utf32_toutf8_test(0x10348, "\xf0\x90\x8d\x88");

#undef utf32_toutf8_test
}

TEST(hex16_fromstr + hex16_tostr) {
	uint32_t v;
	for (v = 0; v <= UINT16_MAX; ++v) {
		char buf[5];
		char dest[4];
		sprintf(buf, "%04x", v);
		assert_equal(hex16_fromstr(buf), v);
		hex16_tostr(dest, v);
		assert_equal(memcmp(dest, buf, 4), 0);
	}
}

TEST(ascii_iscntrl) {
	expect_true(ascii_iscntrl('\0'));
	expect_true(ascii_iscntrl('\t'));
	expect_true(ascii_iscntrl('\n'));
	expect_true(ascii_iscntrl('\r'));
	expect_true(ascii_iscntrl('\x7f'));
	expect_false(ascii_iscntrl(' '));
	expect_false(ascii_iscntrl('3'));
}

TEST(parse + tostring) {
	struct json_node *nodes = NULL;
	size_t nnodes = 0;

	CASE(empty nested arrays) {
		expect_true(json_parse("[]", &nodes, &nnodes));
		expect_equal(nodes[0].val.len, 0);
		expect_equal(json_tostring(nodes), "[]");

		expect_true(json_parse("[ [[[]   ]]]", &nodes, &nnodes));
		expect_equal(nodes[0].val.len, 1);
		expect_equal(nodes[1].val.len, 1);
		expect_equal(nodes[2].val.len, 1);
		expect_equal(nodes[3].val.len, 0);
		expect_equal(json_tostring(nodes), "[[[[]]]]");
	}

	CASE(empty map) {
		expect_true(json_parse("{}", &nodes, &nnodes));
		expect_equal(nodes[0].val.len, 0);
		expect_equal(json_tostring(nodes), "{}");
	}

	CASE(short arrays) {
		expect_true(json_parse(" [      \n42 \n\n,\n true\n\n \n\n]\n  ", &nodes, &nnodes));
		expect_equal(nodes[0].val.len, 2);
		expect_equal(json_tostring(nodes), "[42,true]");
	}

}

TEST(parse_str) {
	char *str;

#define parse_str_test(jsonstr, expstr) do { \
	str = malloc(1 + sizeof jsonstr + 1); \
	strcpy(str, "\"" jsonstr "\""); \
	expect_equal(parse_str(str) - str, sizeof jsonstr + 1); \
	++str; \
	expect_equal(str, expstr); \
} while (0)

	parse_str_test("goat", "goat");
	parse_str_test("t", "t");
	parse_str_test("\\t", "\t");
	parse_str_test("0\\f12", "0\f12");
	parse_str_test("0\\t12\\n\\t\\b345", "0\t12\n\t\b345");
	parse_str_test("\xed\x95\x9cx\t\xf0\x90\x8d\x88", "\xed\x95\x9cx\t\xf0\x90\x8d\x88");
	parse_str_test("\\u0020", " ");
	parse_str_test("\\u20acdc", "\xe2\x82\xac""dc");
	parse_str_test("Ärvíztűrő tükörfúrógép.", "Ärvíztűrő tükörfúrógép.");

#undef parse_str_test
}

TEST(json_writer) {
	struct json_writer w[0];

	json_writer_init(w);

	CASE(json_write_str) {
		CASE(control escape) {
			json_write_str(w, "a\tbcd");
			json_writer_term(w);
			assert_equal(w->buf, "\"a\\tbcd\"");
		}

		CASE(multibyte) {
			json_write_str(w, "\xe0\xa4\xb9");
			json_writer_term(w);
			assert_equal(w->buf, "\"\xe0\xa4\xb9\"");
		}

		CASE(hungarian) {
			json_write_str(w,"Ärvíztűrő \t tükörfúrógép.\n");
			json_write_str(w,"\ntest");
			json_writer_term(w);
			assert_equal(w->buf, "\"Ärvíztűrő \\t tükörfúrógép.\\n\",\"\\ntest\"");
		}
	}

	json_write_beginobj(w);

	json_write_key(w, "a");
	json_write_bool(w, 1);

	json_write_key(w, "b");
	json_write_beginarr(w);
	json_write_beginarr(w);
	json_write_str(w, "\xe0\xa4\xb9");
	json_write_int(w, 7);
	json_write_null(w);
	json_write_num(w, 4.5);
	json_write_endarr(w);
	json_write_endarr(w);

	json_write_endobj(w);

	json_writer_term(w);

	assert_equal(w->buf, "{\"a\":true,\"b\":[[\"\xe0\xa4\xb9\",7,null,4.5]]}");

	json_writer_free(w);

}
