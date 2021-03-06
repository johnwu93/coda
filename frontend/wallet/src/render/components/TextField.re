module Styles = {
  open Css;

  let container =
    style([
      display(`flex),
      alignItems(`center),
      justifyContent(`flexStart),
      height(`rem(2.5)),
      width(`percent(100.)),
      padding2(~v=`zero, ~h=`rem(0.75)),
      background(white),
      border(`px(1), `solid, Theme.Colors.marineAlpha(0.3)),
      borderRadius(`rem(0.25)),
    ]);

  let label =
    merge([
      Theme.Text.smallHeader,
      style([
        textTransform(`uppercase),
        color(Theme.Colors.slateAlpha(0.7)),
      ]),
    ]);

  let input = 
    merge([
      Theme.Text.Body.regular,
      style([
        border(`zero, `solid, white),
        padding(`zero),
        paddingBottom(`px(2)),
        color(Theme.Colors.teal),
        active([outline(`zero, `solid, white)]),
        focus([outline(`zero, `solid, white)]),
      ]),
    ]
  );
};

[@react.component]
let make = (~onChange, ~value, ~label, ~placeholder=?) =>
  <label className=Styles.container>
    <span className=Styles.label> {React.string(label ++ ":")} </span>
    <Spacer width=0.5 />
    <input className=Styles.input type_="text" onChange value ?placeholder />
  </label>;
