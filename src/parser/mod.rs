#[derive(Parser)]
#[grammar = "parser/grammar.pest"] // relative to src
pub struct ButterParser;

#[cfg(test)]
mod tests {
    use std::result::Result::Err;

    use super::*;

    use pest::Parser;

    macro_rules! assert_result_pairs {
        ($result:ident, $expected:expr) => {
            assert!(matches!($result, Ok(_)));
            let pairs = $result.unwrap();
            assert_pairs!(pairs, $expected);
        };
    }

    macro_rules! assert_pairs {
        ($pairs:expr, $expected:expr) => {
            let mut pairs_clone = $pairs.clone();
            assert_eq!(
                pairs_clone.clone().count(),
                $expected.len(),
                "Pairs were: {}",
                pairs_clone
            );
            for (rule, value) in $expected {
                let pair = pairs_clone.next().unwrap();
                assert_pair!(pair, rule, value);
            }
        };
    }

    macro_rules! assert_pair {
        ($pair:ident, $expectedRule:expr, $expectedValue:expr) => {
            assert_eq!($pair.as_rule(), $expectedRule);
            assert_eq!($pair.as_str(), $expectedValue);
        };
    }

    macro_rules! assert_rules {
        ($pairs:expr, $expected:expr) => {
            let mut pairs_clone = $pairs.clone();
            assert_eq!(
                pairs_clone.clone().count(),
                $expected.len(),
                "Pairs were: {}",
                pairs_clone
            );
            for rule in $expected {
                let pair = pairs_clone.next().unwrap();
                assert_rule!(pair, rule);
            }
        };
    }

    macro_rules! assert_rule {
        ($pair:expr, $expected:expr) => {
            assert_eq!($pair.as_rule(), $expected);
        };
    }

    #[test]
    fn test_package() {
        let result = ButterParser::parse(Rule::package_statement, "{ package s0me.package}");

        assert_result_pairs!(result, vec![(Rule::package_name, "s0me.package")]);
    }

    #[test]
    fn test_comment_is_ignored_on_own_line() {
        let result = ButterParser::parse(
            Rule::package_statement,
            "{package
          // some comment text
          s0me.package}",
        );

        assert_result_pairs!(result, vec![(Rule::package_name, "s0me.package")]);
    }

    #[test]
    fn test_comment_is_ignored_at_end_of_line() {
        let result = ButterParser::parse(
            Rule::package_statement,
            "{package // some comment text
          s0me.package}",
        );

        assert_result_pairs!(result, vec![(Rule::package_name, "s0me.package")]);
    }

    #[test]
    fn test_import_all_from_package() {
        let result = ButterParser::parse(
            Rule::import_statement,
            "import * as things from './a/file.btr'",
        );

        let mut pairs = result.unwrap();
        assert_rules!(pairs, vec![Rule::import_statement]);
        let mut inner_pairs = pairs.next().unwrap().into_inner();
        assert_pairs!(
            inner_pairs,
            vec![
                (Rule::import_wild, "* as things"),
                (Rule::import_source, "'./a/file.btr'"),
            ]
        );
        let wild_pairs = inner_pairs.next().unwrap().into_inner();
        assert_pairs!(wild_pairs, vec![(Rule::import_rename, "things")]);
        // Currently 'import_path' is not extractable from import_source for some reason.
    }

    #[test]
    fn test_import_items_from_package() {
        let result = ButterParser::parse(
            Rule::import_statement,
            "import {thing as t, other} from './a/file.btr'",
        );

        let mut pairs = result.unwrap();
        assert_rules!(pairs, vec![Rule::import_statement]);
        let mut inner_pairs = pairs.next().unwrap().into_inner();
        assert_pairs!(
            inner_pairs,
            vec![
                (Rule::import_items, "thing as t, other"),
                (Rule::import_source, "'./a/file.btr'"),
            ]
        );
        let item_pairs = inner_pairs.next().unwrap().into_inner();
        assert_pairs!(
            item_pairs,
            vec![
                (Rule::import_item, "thing"),
                (Rule::import_rename, "t"),
                (Rule::import_items, "other"),
            ]
        );
        let other_items = item_pairs.last().unwrap().into_inner();
        assert_pairs!(other_items, vec![(Rule::import_item, "other")]);
    }

    #[test]
    fn test_html_block_statement() {
        let result = ButterParser::parse(
            Rule::block_statement,
            "{html template()}
        <div class=\"foo\">
          <p>{lb}bar{rb}</p>
        </div>
        {/}",
        );

        let mut pairs = result.unwrap();
        assert_rules!(pairs, vec![Rule::block_statement]);
        let mut block_pairs = pairs.next().unwrap().into_inner();
        assert_pairs!(
            block_pairs,
            vec![
                (Rule::block_type, "html"),
                (Rule::identifier, "template"),
                (
                    Rule::block_content,
                    "<div class=\"foo\">
          <p>{lb}bar{rb}</p>
        </div>
        "
                )
            ]
        );
        _ = block_pairs.next();
        _ = block_pairs.next();
        let mut content_pairs = block_pairs.next().unwrap().into_inner();
        assert_pairs!(
            content_pairs,
            vec![
                (Rule::html_tag_open_or_void, "<div class=\"foo\">"),
                (Rule::html_tag_open_or_void, "<p>"),
                (Rule::inner_statement, "{lb}"),
                (Rule::html_text, "bar"),
                (Rule::inner_statement, "{rb}"),
                (Rule::html_tag_close, "</p>"),
                (Rule::html_tag_close, "</div>"),
            ]
        );
        let mut div_pairs = content_pairs.next().unwrap().into_inner();
        assert_pairs!(
            div_pairs,
            vec![
                (Rule::identifier, "div"),
                (Rule::html_attribute, "class=\"foo\""),
            ]
        );
        let p_pairs = content_pairs.next().unwrap().into_inner();
        assert_pairs!(p_pairs, vec![(Rule::identifier, "p"),]);
    }

    #[test]
    fn test_html_void_tag_with_attributes() {
        let result = ButterParser::parse(Rule::html_content, "<input id=\"id\" />");

        let mut pairs = result.unwrap();
        assert_rules!(pairs, vec![Rule::html_tag_void]);
        let mut tag_pairs = pairs.next().unwrap().into_inner();
        assert_pairs!(
            tag_pairs,
            vec![
                (Rule::identifier, "input"),
                (Rule::html_attribute, "id=\"id\""),
            ]
        );
    }

    #[test]
    fn test_html_tag_with_attributes() {
        let result = ButterParser::parse(
            Rule::html_content,
            "<h1 class=\"foo {rb}bar\" {someCall()}>contents</h1>",
        );

        let mut pairs = result.unwrap();
        assert_pairs!(
            pairs,
            vec![
                (
                    Rule::html_tag_open_or_void,
                    "<h1 class=\"foo {rb}bar\" {someCall()}>"
                ),
                (Rule::html_text, "contents"),
                (Rule::html_tag_close, "</h1>"),
            ]
        );
        let mut tag_pairs = pairs.next().unwrap().into_inner();
        assert_pairs!(
            tag_pairs,
            vec![
                (Rule::identifier, "h1"),
                (Rule::html_attribute, "class=\"foo {rb}bar\""),
                (Rule::html_attribute, "{someCall()}"),
            ]
        );
        _ = tag_pairs.next();
        let mut attr_pairs = tag_pairs.next().unwrap().into_inner();
        assert_pairs!(
            attr_pairs,
            vec![
                (Rule::identifier, "class"),
                (Rule::html_attribute_value, "foo {rb}bar"),
            ]
        );
        _ = attr_pairs.next();
        let attr_values = attr_pairs.next().unwrap().into_inner();
        assert_pairs!(
            attr_values,
            vec![
                (Rule::html_attribute_plain_value, "foo "),
                (Rule::inner_statement, "{rb}"),
                (Rule::html_attribute_plain_value, "bar"),
            ]
        );
        let mut attr_pairs = tag_pairs.next().unwrap().into_inner();
        assert_pairs!(attr_pairs, vec![(Rule::inner_statement, "{someCall()}")]);
        let statement_pairs = attr_pairs.next().unwrap().into_inner();
        assert_pairs!(statement_pairs, vec![(Rule::call, "someCall()")]);
    }

    #[test]
    fn test_identifier() {
        let result = ButterParser::parse(Rule::identifier, "abc123");
        assert_result_pairs!(result, vec![(Rule::identifier, "abc123")]);
        let result = ButterParser::parse(Rule::identifier, "1abc");
        assert!(matches!(result, Err(_)));
        let result = ButterParser::parse(Rule::identifier, "a c");
        assert_result_pairs!(result, vec![(Rule::identifier, "a")]);
    }

    macro_rules! test_parse_error {
        ($name:ident, $rule:expr, $value:expr) => {
            #[test]
            fn $name() {
                let result = ButterParser::parse($rule, $value);

                assert!(matches!(result, Err(_)));
            }
        };
    }

    test_parse_error!(
        test_package_name_must_start_with_letter,
        Rule::package_statement,
        "{package 0me.package}"
    );

    test_parse_error!(
        test_main_must_start_with_package,
        Rule::main,
        "{html tmpl}<p>yo</p>{/}
      {package some.package}"
    );
}
