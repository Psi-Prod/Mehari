let parse s g = compare (Mehari.Gemtext.of_string s) g |> Int.equal 0

let parsing () =
  let open Mehari.Gemtext in
  assert (parse "#Foo" [ heading `H1 "Foo" ]);
  assert (parse "# Foo" [ heading `H1 "Foo" ]);
  assert (parse "##Foo" [ heading `H2 "Foo" ]);
  assert (parse "## Foo" [ heading `H2 "Foo" ]);
  assert (parse "###Foo" [ heading `H3 "Foo" ]);
  assert (parse "### Foo" [ heading `H3 "Foo" ]);
  assert (parse "#### Foo" [ heading `H3 "# Foo" ]);
  assert (parse " # Foo" [ text " # Foo" ]);
  assert (parse "*Bar" [ text "*Bar" ]);
  assert (parse "* Bar" [ list_item "Bar" ]);
  assert (parse "* *Bar" [ list_item "*Bar" ]);
  assert (parse "**Bar" [ text "**Bar" ]);
  assert (parse ">Foo" [ quote "Foo" ]);
  assert (parse "> Foo" [ quote " Foo" ]);
  assert (
    parse "=> gemini://heyplzlookat.me/" [ link "gemini://heyplzlookat.me/" ]);
  assert (
    parse "=> gemini://heyplzlookat.me/about HeyPlzLookAtMe"
      [ link "gemini://heyplzlookat.me/about" ~name:"HeyPlzLookAtMe" ]);
  assert (
    parse "  =>\t \thttps://docs.heyplzlookat.me Docs"
      [ link "https://docs.heyplzlookat.me" ~name:"Docs" ]);
  assert (parse "=>foo Bar" [ link "foo" ~name:"Bar" ]);
  assert (parse "```\n#foo\n```" [ preformat "#foo\n" ]);
  assert (parse "```foo\n# Bar  \n```hello" [ preformat ~alt:"foo" "# Bar  \n" ])

let show g s = compare s (Mehari.Gemtext.to_string g) |> Int.equal 0

let pretty_printing () =
  let open Mehari.Gemtext in
  assert (show [ text "Foo" ] "Foo");
  assert (show [ link "heyplzlookat.me" ] "=> heyplzlookat.me");
  assert (
    show
      [ link "heyplzlookat.me" ~name:"HeyPlzLookAtMe" ]
      "=> heyplzlookat.me HeyPlzLookAtMe");
  assert (show [ heading `H1 "Foo" ] "# Foo");
  assert (show [ heading `H2 "Foo" ] "## Foo");
  assert (show [ heading `H3 "Foo" ] "### Foo");
  assert (show [ list_item "Bar" ] "* Bar");
  assert (show [ quote "Foo" ] ">Foo");
  assert (show [ preformat "( ´ ▽ ` )" ] "```\n( ´ ▽ ` )\n```");
  assert (show [ preformat ~alt:"japon" "( ´ ▽ ` )" ] "```japon\n( ´ ▽ ` )\n```")

let pass () =
  parsing ();
  pretty_printing ()
