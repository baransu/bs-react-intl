type state = {locale: Locale.locale};

type action =
  | SetLocale(Locale.locale);

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (_state, action) =>
        switch (action) {
        | SetLocale(locale) => {locale: locale}
        },
      {locale: Locale.En},
    );

  <ReactIntl.IntlProvider
    locale={state.locale->Locale.toString}
    messages={state.locale->Locale.toMessages->ReactIntl.messagesArrayToDict}>
    <Page
      locale={state.locale}
      setLocale={locale => locale->SetLocale->dispatch}
    />
  </ReactIntl.IntlProvider>;
};
