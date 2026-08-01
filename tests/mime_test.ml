open Mehari

let fake_source =
  {|#!/bin/node
import * from "/modules/module.js";

async function() {
  for (const i = 0; i < 9; i++) {
    print(i);
  }
}|}

let test_empty_mime =
  let open Alcotest in
  test_case "empty MIME type test" `Quick (fun () ->
      check_raises
        "when trying to create an empty MIME type, Invalid_argument is raised"
        (Invalid_argument "Mehari.Mime.make") (fun () -> ignore @@ Mime.make ""))

let test_predefined_mime =
  let open Alcotest in
  test_case "predefined MIME types value test" `Quick (fun () ->
      let expected =
        ( "application/octet-stream",
          "text/plain; charset=utf-8",
          "text/markdown; charset=utf-8" )
      in
      let computed =
        Mime.
          ( to_string app_octet_stream,
            to_string plaintext,
            to_string @@ text "markdown" )
      in
      check (triple string string string) "should be equal" expected computed)

let test_gemini_mime_1 =
  let open Alcotest in
  test_case "test Gemini MIME type - 1" `Quick (fun () ->
      let expected = "text/gemini; charset=utf-8" in
      let computed = Mime.gemini () |> Mime.to_string in
      check string "should be equal" expected computed)

let test_gemini_mime_2 =
  let open Alcotest in
  test_case "test Gemini MIME type - 2" `Quick (fun () ->
      let expected = "text/gemini; charset=ISO-8859-1" in
      let computed = Mime.gemini ~charset:"ISO-8859-1" () |> Mime.to_string in
      check string "should be equal" expected computed)

let test_gemini_mime_3 =
  let open Alcotest in
  test_case "test Gemini MIME type - 3" `Quick (fun () ->
      let expected = "text/gemini; charset=ISO-8859-1; lang=\"en,fr\"" in
      let computed =
        Mime.gemini ~charset:"ISO-8859-1" ~lang:[ "en"; "fr" ] ()
        |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_gemini_mime_4 =
  let open Alcotest in
  test_case "test Gemini MIME type - 4" `Quick (fun () ->
      let expected = "text/gemini; charset=utf-16; lang=fr" in
      let computed =
        Mime.gemini ~charset:"utf-16" ~lang:[ "fr" ] () |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_mime_1 =
  let open Alcotest in
  test_case "test MIME type - 1" `Quick (fun () ->
      let expected = "audio/mp3" in
      let computed = Mime.make "audio/mp3" |> Mime.to_string in
      check string "should be equal" expected computed)

let test_mime_2 =
  let open Alcotest in
  test_case "test MIME type - 2" `Quick (fun () ->
      let expected = "text/html; charset=utf-8" in
      let computed = Mime.make "text/html" |> Mime.to_string in
      check string "should be equal" expected computed)

let test_mime_3 =
  let open Alcotest in
  test_case "test MIME type - 3" `Quick (fun () ->
      let expected = "text/calendar; charset=us-ascii" in
      let computed =
        Mime.make ~charset:"us-ascii" "text/calendar" |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_mime_4 =
  let open Alcotest in
  test_case "test MIME type - 4" `Quick (fun () ->
      let expected = "image/gif; charset=ISO-8859-1" in
      let computed =
        Mime.make ~charset:"ISO-8859-1" "image/gif" |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_mime_5 =
  let open Alcotest in
  test_case "test MIME type - 5" `Quick (fun () ->
      let expected = "application/json; charset=latin1" in
      let computed =
        Mime.make "application/json"
        |> Mime.with_charset "latin1" |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_mime_6 =
  let open Alcotest in
  test_case "test MIME type - 6" `Quick (fun () ->
      let expected = "text/gemini; charset=Windows-1252; lang=zh-Hans-CN" in
      let computed =
        Mime.gemini ~lang:[ "zh-Hans-CN" ] ()
        |> Mime.with_charset "Windows-1252"
        |> Mime.to_string
      in
      check string "should be equal" expected computed)

let test_mime_inference_1 =
  let open Alcotest in
  test_case "test MIME type inference - 1" `Quick (fun () ->
      let expected =
        Some "application/vnd.oasis.opendocument.text; charset=Shift_JIS"
      in
      let computed =
        Mime.from_filename ~charset:"Shift_JIS" "document.odt"
        |> Option.map Mime.to_string
      in
      check (option string) "should be equal" expected computed)

let test_mime_inference_2 =
  let open Alcotest in
  test_case "test MIME type inference - 2" `Quick (fun () ->
      let expected = Some "application/zip" in
      let computed =
        Mime.from_filename "archive.zip" |> Option.map Mime.to_string
      in
      check (option string) "should be equal" expected computed)

let test_mime_inference_3 =
  let open Alcotest in
  test_case "test MIME type inference - 3" `Quick (fun () ->
      let expected = Some "application/javascript" in
      let computed =
        Mime.from_content ~tree:Conan_javascript.tree fake_source
        |> Option.map Mime.to_string
      in
      check (option string) "should be equal" expected computed)

let test_mime_inference_4 =
  let open Alcotest in
  test_case "test MIME type inference - 4" `Quick (fun () ->
      let expected = Some "application/javascript; charset=us-ascii" in
      let computed =
        Mime.from_content ~charset:"us-ascii" ~tree:Conan_javascript.tree
          fake_source
        |> Option.map Mime.to_string
      in
      check (option string) "should be equal" expected computed)

let cases =
  ( "mime_test",
    [
      test_empty_mime;
      test_predefined_mime;
      test_gemini_mime_1;
      test_gemini_mime_2;
      test_gemini_mime_3;
      test_gemini_mime_4;
      test_mime_1;
      test_mime_2;
      test_mime_3;
      test_mime_4;
      test_mime_5;
      test_mime_6;
      test_mime_inference_1;
      test_mime_inference_2;
      test_mime_inference_3;
      test_mime_inference_4;
    ] )
