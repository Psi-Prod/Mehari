module Gemtext = Mehari.Gemtext

let gemtext =
  let pp ppf t =
    let pp_line ppf = function
      | Gemtext.Text t -> Fmt.pf ppf "Text %S" t
      | Link { url; name } ->
          Fmt.pf ppf "Link { url = %S; name = %a }" url
            Fmt.Dump.(option string)
            name
      | Preformat { alt; text } ->
          Fmt.pf ppf "Preformat { alt = %a; text = %S }"
            Fmt.Dump.(option string)
            alt text
      | Heading (`H1, h) -> Fmt.pf ppf "Heading (`H1, %S)" h
      | Heading (`H2, h) -> Fmt.pf ppf "Heading (`H2, %S)" h
      | Heading (`H3, h) -> Fmt.pf ppf "Heading (`H3, %S)" h
      | List_item i -> Fmt.pf ppf "List_item %S" i
      | Quote q -> Fmt.pf ppf "Quote %S" q
    in
    Fmt.Dump.list pp_line ppf t
  in
  Alcotest.testable pp Gemtext.equal

let doc1 =
  {|#Foo
# Foo
##Foo
## Foo
###Foo
### Foo
 # Foo
#### gros piège
*Bar
 *Bar
* Bar
 * Bar
* *Bar
**Bar
>Foo
> Foo
 > Foo
>>>hééhéhhé
=> gemini://heyplzlookat.me/
=> gemini://heyplzlookat.me/about HeyPlzLookAtMe
=>	 	https://docs.heyplzlookat.me Docs
=>foo Bar

```
#foo
```
```foo
# Bar

```|}

let doc2 =
  {|# Thought on Gemtext markup
* Date: February 2021
* Tags: gemini, reviews

## Introduction

> The format permits richer typographic possibilities than the plain text of Gopher, but remains extremely easy to parse.
Here is an example of a Python parser that demonstrates the truth of that statement:
```python
…
```

End of the article with a newline.
|}

let doc3 =
  {|>Foo
sous gros test
=> heyplzlookat.me
=> heyplzlookat.me HeyPlzLookAtMe
#  Foo
## Foo
### Foo
* *** Bar
>Karl Marx
><><>fais pas bleh
```
( ´ ▽ ` )
```
```japon
( ´ ▽ ` )
```|}

let doc4 =
  {|# Real Software Artisan
## Mehari

Mehari is our pure OCaml Gemini server. It runs this site and supports various features such as:

* MIME type inference
* Static files serving
* Rate limiting
* Virtual hosting using Server Name Indication (SNI)
* CGI
* Long running TCP connection for response body streaming.

It is also cross-platform and supports Eio with OCaml >= 5.0.0 as well as Mirage OS and UNIX.

=>   https://github.com/Psi-Prod/Mehari
=>  https://docs.heyplzlookat.me/mehari/
|}

let test_parsing_1 =
  let open Alcotest in
  test_case "Gemtext parsing test - 1" `Quick (fun () ->
      let expected =
        Gemtext.
          [
            heading `H1 "Foo";
            heading `H1 "Foo";
            heading `H2 "Foo";
            heading `H2 "Foo";
            heading `H3 "Foo";
            heading `H3 "Foo";
            text " # Foo";
            heading `H3 "# gros piège";
            text "*Bar";
            text " *Bar";
            list_item "Bar";
            text " * Bar";
            list_item "*Bar";
            text "**Bar";
            quote "Foo";
            quote " Foo";
            text " > Foo";
            quote ">>hééhéhhé";
            link "gemini://heyplzlookat.me/";
            link "gemini://heyplzlookat.me/about" ~name:"HeyPlzLookAtMe";
            link "https://docs.heyplzlookat.me" ~name:"Docs";
            link "foo" ~name:"Bar";
            text "";
            preformat "#foo";
            preformat ~alt:"foo" "# Bar\n";
          ]
      in
      let computed = Gemtext.of_string doc1 in
      check gemtext "should be equal" expected computed)

let test_parsing_2 =
  let open Alcotest in
  test_case "Gemtext parsing test - 2" `Quick (fun () ->
      let expected =
        Gemtext.
          [
            heading `H1 "Thought on Gemtext markup";
            list_item "Date: February 2021";
            list_item "Tags: gemini, reviews";
            newline;
            heading `H2 "Introduction";
            newline;
            quote
              " The format permits richer typographic possibilities than the \
               plain text of Gopher, but remains extremely easy to parse.";
            text
              "Here is an example of a Python parser that demonstrates the \
               truth of that statement:";
            preformat "…" ~alt:"python";
            text "";
            text "End of the article with a newline.";
            text "";
          ]
      in
      let computed = Gemtext.of_string doc2 in
      check gemtext "should be equal" expected computed)

let test_parsing_3 =
  let open Alcotest in
  test_case "Gemtext parsing test - 3" `Quick (fun () ->
      let expected =
        Gemtext.
          [
            quote "Foo";
            text "sous gros test";
            link "heyplzlookat.me";
            link "heyplzlookat.me" ~name:"HeyPlzLookAtMe";
            heading `H1 "Foo";
            heading `H2 "Foo";
            heading `H3 "Foo";
            list_item "*** Bar";
            quote "Karl Marx";
            quote "<><>fais pas bleh";
            preformat "( ´ ▽ ` )";
            preformat ~alt:"japon" "( ´ ▽ ` )";
          ]
      in
      let computed = Gemtext.of_string doc3 in
      check gemtext "should be equal" expected computed)

let test_parsing_4 =
  let open Alcotest in
  test_case "Gemtext parsing test - 4" `Quick (fun () ->
      let expected =
        Gemtext.
          [
            heading `H1 "Real Software Artisan";
            heading `H2 "Mehari";
            text "";
            text
              "Mehari is our pure OCaml Gemini server. It runs this site and \
               supports various features such as:";
            text "";
            list_item "MIME type inference";
            list_item "Static files serving";
            list_item "Rate limiting";
            list_item "Virtual hosting using Server Name Indication (SNI)";
            list_item "CGI";
            list_item "Long running TCP connection for response body streaming.";
            text "";
            text
              "It is also cross-platform and supports Eio with OCaml >= 5.0.0 \
               as well as Mirage OS and UNIX.";
            text "";
            link "https://github.com/Psi-Prod/Mehari";
            link "https://docs.heyplzlookat.me/mehari/";
            text "";
          ]
      in
      let computed = Gemtext.of_string doc4 in
      check gemtext "should be equal" expected computed)

let test_printing_1 =
  let open Alcotest in
  test_case "Gemtext priting test - 1" `Quick (fun () ->
      let expected = doc2 in
      let computed =
        Gemtext.(
          to_string
            [
              heading `H1 "Thought on Gemtext markup";
              list_item "Date: February 2021";
              list_item "Tags: gemini, reviews";
              text "";
              heading `H2 "Introduction";
              text "";
              quote
                " The format permits richer typographic possibilities than the \
                 plain text of Gopher, but remains extremely easy to parse.";
              text
                "Here is an example of a Python parser that demonstrates the \
                 truth of that statement:";
              preformat ~alt:"python" "…";
              text "";
              text "End of the article with a newline.";
              text "";
            ])
      in
      check string "should be equal" expected computed)

let test_printing_2 =
  let open Alcotest in
  test_case "Gemtext printing test - 2" `Quick (fun () ->
      let expected = doc3 in
      let computed =
        Gemtext.(
          to_string
            [
              text ">Foo";
              text "sous gros test";
              link "heyplzlookat.me";
              link "heyplzlookat.me" ~name:"HeyPlzLookAtMe";
              heading `H1 " Foo";
              heading `H2 "Foo";
              heading `H3 "Foo";
              list_item "*** Bar";
              quote "Karl Marx";
              quote "<><>fais pas bleh";
              preformat "( ´ ▽ ` )";
              preformat ~alt:"japon" "( ´ ▽ ` )";
            ])
      in
      check string "should be equal" expected computed)

let cases =
  ( "gemtext_test",
    [
      test_parsing_1;
      test_parsing_2;
      test_parsing_3;
      test_parsing_4;
      test_printing_1;
      test_printing_2;
    ] )
