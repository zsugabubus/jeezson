#include <test.h>

#include "jeezson.h"
#include "jeezson.c"

TEST(wtf) {
}
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
    char dest[5];

    CASE(nonoverlapping) {
#define utf8_chrcpy_test(n, src) do { \
    expect_equal(n, utf8_chrcpy(dest, src)); \
    dest[n] = '\0'; \
    expect_equal(&dest, src); \
} while (0)

        CASE(one)
            utf8_chrcpy_test(1, "\x24");

        CASE(two)
            utf8_chrcpy_test(2, "\xc2\xa2");

        CASE(three) {
            utf8_chrcpy_test(3, "\xe0\xa4\xb9");
            utf8_chrcpy_test(3, "\xe2\x82\xac");
            utf8_chrcpy_test(3, "\xed\x95\x9c");
        }

        CASE(four)
            utf8_chrcpy_test(4, "\xf0\x90\x8d\x88");
#undef utf8_chrcpy_test
    }
    /* TODO: Test overlapping. */
}

TEST(utf32_toutf8) {
    char dest[4];

#define utf32_toutf8_test(cp, utf8str) do { \
    dest[utf32_toutf8(dest, cp)] = '\0'; \
    expect_equal(&dest, utf8str); \
} while (0)

    CASE(one)
        utf32_toutf8_test(0x24, "\x24");

    CASE(two)
        utf32_toutf8_test(0xa2, "\xc2\xa2");

    CASE(three) {
        utf32_toutf8_test(0x0939, "\xe0\xa4\xb9");
        utf32_toutf8_test(0x20ac, "\xe2\x82\xac");
        utf32_toutf8_test(0xd55c, "\xed\x95\x9c");
    }

    CASE(four)
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

TEST(json_writer) {
    struct json_writer w[0];

    json_writer_init(w);

    CASE(json_write_str) {
        CASE(simple escape) {
            json_write_str(w, "a\tbcd");
            json_writer_term(w);
            assert_equal(w->buf, "\"a\\tbcd\"");
        }

        CASE(three) {
            json_write_str(w, "\xe0\xa4\xb9");
            json_writer_term(w);
            assert_equal(w->buf, "\"\xe0\xa4\xb9\"");
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
