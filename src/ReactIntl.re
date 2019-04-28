open Belt;

/* Common */
type message = {
  .
  "id": string,
  "defaultMessage": string,
};

type jsonMessage = {
  .
  "id": string,
  "defaultMessage": string,
  "message": Js.nullable(string),
};

type jsonMessages = array(jsonMessage);

/* addLocaleData */
type localeData('t) = {.. "locale": string} as 't;

[@bs.module "react-intl"] external addLocaleData: localeData('t) => unit = "";

/* defineMessages */
type defineMessages('m) = (. 'm) => 'm;

[@bs.module "react-intl"]
external defineMessages: defineMessages(Js.t({..})) = "";

/* Formatters */
type localeMatcher =
  | BestFitLocaleMatcher
  | LookupLocaleMatcher;

let mapReasonLocaleMatcherToJs = localeMatcher =>
  Option.map(localeMatcher, localeMatcher =>
    switch (localeMatcher) {
    | BestFitLocaleMatcher => "best fit"
    | LookupLocaleMatcher => "lookup"
    }
  );

type formatMatcher =
  | BestFitFormatMatcher
  | BasicFormatMatcher;

let mapReasonFormatMatcherToJs = formatMatcher =>
  Option.map(formatMatcher, formatMatcher =>
    switch (formatMatcher) {
    | BestFitFormatMatcher => "best fit"
    | BasicFormatMatcher => "basic"
    }
  );

type numeralFormat =
  | NumericNumeralFormat
  | TwoDigitNumeralFormat;

let mapReasonNumeralFormatToJs = numeralFormat =>
  Option.map(numeralFormat, numeralFormat =>
    switch (numeralFormat) {
    | NumericNumeralFormat => "numeric"
    | TwoDigitNumeralFormat => "2-digit"
    }
  );

type textualFormat =
  | NarrowTextualFormat
  | ShortTextualFormat
  | LongTextualFormat;

let mapReasonTextualFormatToJs = textualFormat =>
  Option.map(textualFormat, textualFormat =>
    switch (textualFormat) {
    | NarrowTextualFormat => "narrow"
    | ShortTextualFormat => "short"
    | LongTextualFormat => "long"
    }
  );

type mixedFormat =
  | NumericMixedFormat
  | TwoDigitMixedFormat
  | NarrowMixedFormat
  | ShortMixedFormat
  | LongMixedFormat;

let mapReasonMixedFormatToJs = mixedFormat =>
  Option.map(mixedFormat, mixedFormat =>
    switch (mixedFormat) {
    | NarrowMixedFormat => "narrow"
    | ShortMixedFormat => "short"
    | LongMixedFormat => "long"
    | NumericMixedFormat => "numeric"
    | TwoDigitMixedFormat => "2-digit"
    }
  );

type timeZoneName =
  | ShortTimeZoneName
  | LongTimeZoneName;

let mapReasonTimeZoneNameToJs = timeZoneName =>
  Option.map(timeZoneName, timeZoneName =>
    switch (timeZoneName) {
    | ShortTimeZoneName => "short"
    | LongTimeZoneName => "long"
    }
  );

type dateTimeFormatOptionsRe = {
  .
  "localeMatcher": option(localeMatcher),
  "formatMatcher": option(formatMatcher),
  "timeZone": option(string),
  "hour12": option(bool),
  "weekday": option(textualFormat),
  "era": option(textualFormat),
  "year": option(numeralFormat),
  "month": option(mixedFormat),
  "day": option(numeralFormat),
  "hour": option(numeralFormat),
  "minute": option(numeralFormat),
  "second": option(numeralFormat),
  "timeZoneName": option(timeZoneName),
  "format": option(string),
};

type dateTimeFormatOptionsJs = {
  .
  "localeMatcher": Js.nullable(string),
  "formatMatcher": Js.nullable(string),
  "timeZone": Js.nullable(string),
  "hour12": Js.nullable(bool),
  "weekday": Js.nullable(string),
  "era": Js.nullable(string),
  "year": Js.nullable(string),
  "month": Js.nullable(string),
  "day": Js.nullable(string),
  "hour": Js.nullable(string),
  "minute": Js.nullable(string),
  "second": Js.nullable(string),
  "timeZoneName": Js.nullable(string),
  "format": Js.nullable(string),
};

let mapReasonDateTimeFormatOptionsToJs =
    (options: dateTimeFormatOptionsRe): dateTimeFormatOptionsJs => {
  "localeMatcher":
    options##localeMatcher
    |> mapReasonLocaleMatcherToJs
    |> Js.Nullable.fromOption,
  "formatMatcher":
    options##formatMatcher
    |> mapReasonFormatMatcherToJs
    |> Js.Nullable.fromOption,
  "timeZone": options##timeZone |> Js.Nullable.fromOption,
  "hour12": options##hour12 |> Js.Nullable.fromOption,
  "weekday":
    options##weekday |> mapReasonTextualFormatToJs |> Js.Nullable.fromOption,
  "era": options##era |> mapReasonTextualFormatToJs |> Js.Nullable.fromOption,
  "year":
    options##year |> mapReasonNumeralFormatToJs |> Js.Nullable.fromOption,
  "month":
    options##month |> mapReasonMixedFormatToJs |> Js.Nullable.fromOption,
  "day": options##day |> mapReasonNumeralFormatToJs |> Js.Nullable.fromOption,
  "hour":
    options##hour |> mapReasonNumeralFormatToJs |> Js.Nullable.fromOption,
  "minute":
    options##minute |> mapReasonNumeralFormatToJs |> Js.Nullable.fromOption,
  "second":
    options##second |> mapReasonNumeralFormatToJs |> Js.Nullable.fromOption,
  "timeZoneName":
    options##timeZoneName
    |> mapReasonTimeZoneNameToJs
    |> Js.Nullable.fromOption,
  "format": options##format |> Js.Nullable.fromOption,
};

type relativeStyle =
  | BestFitRelativeStyle
  | NumericRelativeStyle;

let mapReasonRelativeStyleToJs = relativeStyle =>
  Option.map(relativeStyle, relativeStyle =>
    switch (relativeStyle) {
    | BestFitRelativeStyle => "best fit"
    | NumericRelativeStyle => "numeric"
    }
  );

type units =
  | Second
  | Minute
  | Hour
  | Day
  | Month
  | Year;

let mapReasonUnitsToJs = units =>
  Option.map(units, units =>
    switch (units) {
    | Second => "second"
    | Minute => "minute"
    | Hour => "hour"
    | Day => "day"
    | Month => "month"
    | Year => "year"
    }
  );

type relativeFormatOptionsRe = {
  .
  "style": option(relativeStyle),
  "units": option(units),
  "format": option(string),
  "now": option(int),
};

type relativeFormatOptionsJs = {
  .
  "style": Js.nullable(string),
  "units": Js.nullable(string),
  "format": Js.nullable(string),
  "now": Js.nullable(int),
};

let mapReasonRelativeFormatOptionsToJs =
    (options: relativeFormatOptionsRe): relativeFormatOptionsJs => {
  "style":
    options##style |> mapReasonRelativeStyleToJs |> Js.Nullable.fromOption,
  "units": options##units |> mapReasonUnitsToJs |> Js.Nullable.fromOption,
  "format": options##format |> Js.Nullable.fromOption,
  "now": options##now |> Js.Nullable.fromOption,
};

type numberStyle =
  | DecimalNumberStyle
  | CurrencyNumberStyle
  | PercentNumberStyle;

let mapReasonNumberStyleToJs = numberStyle =>
  Option.map(numberStyle, numberStyle =>
    switch (numberStyle) {
    | DecimalNumberStyle => "decimal"
    | CurrencyNumberStyle => "currency"
    | PercentNumberStyle => "percent"
    }
  );

type currencyDisplay =
  | SymbolCurrencyDisplay
  | CodeCurrencyDisplay
  | NameCurrencyDisplay;

let mapReasonCurrencyDisplayToJs = currencyDisplay =>
  Option.map(currencyDisplay, currencyDisplay =>
    switch (currencyDisplay) {
    | SymbolCurrencyDisplay => "symbol"
    | CodeCurrencyDisplay => "code"
    | NameCurrencyDisplay => "name"
    }
  );

type numberFormatOptionsRe = {
  .
  "localeMatcher": option(localeMatcher),
  "style": option(numberStyle),
  "currency": option(string),
  "currencyDisplay": option(currencyDisplay),
  "useGrouping": option(bool),
  "minimumIntegerDigits": option(int),
  "minimumFractionDigits": option(int),
  "maximumFractionDigits": option(int),
  "minimumSignificantDigits": option(int),
  "maximumSignificantDigits": option(int),
};

type numberFormatOptionsJs = {
  .
  "localeMatcher": Js.nullable(string),
  "style": Js.nullable(string),
  "currency": Js.nullable(string),
  "currencyDisplay": Js.nullable(string),
  "useGrouping": Js.nullable(bool),
  "minimumIntegerDigits": Js.nullable(int),
  "minimumFractionDigits": Js.nullable(int),
  "maximumFractionDigits": Js.nullable(int),
  "minimumSignificantDigits": Js.nullable(int),
  "maximumSignificantDigits": Js.nullable(int),
};

let mapReasonNumberFormatOptionsToJs = options => {
  "localeMatcher":
    options##localeMatcher
    |> mapReasonLocaleMatcherToJs
    |> Js.Nullable.fromOption,
  "style":
    options##style |> mapReasonNumberStyleToJs |> Js.Nullable.fromOption,
  "currency": options##currency |> Js.Nullable.fromOption,
  "currencyDisplay":
    options##currencyDisplay
    |> mapReasonCurrencyDisplayToJs
    |> Js.Nullable.fromOption,
  "useGrouping": options##useGrouping |> Js.Nullable.fromOption,
  "minimumIntegerDigits":
    options##minimumIntegerDigits |> Js.Nullable.fromOption,
  "minimumFractionDigits":
    options##minimumFractionDigits |> Js.Nullable.fromOption,
  "maximumFractionDigits":
    options##maximumFractionDigits |> Js.Nullable.fromOption,
  "minimumSignificantDigits":
    options##minimumSignificantDigits |> Js.Nullable.fromOption,
  "maximumSignificantDigits":
    options##maximumSignificantDigits |> Js.Nullable.fromOption,
};

type pluralStyle =
  | CardinalPluralStyle
  | OrdinalPluralStyle;

let mapReasonPluralStyleToJs = pluralStyle =>
  Js.Option.map(
    (. pluralStyle) =>
      switch (pluralStyle) {
      | CardinalPluralStyle => "cardinal"
      | OrdinalPluralStyle => "ordinal"
      },
    pluralStyle,
  )
  |> Js.Nullable.fromOption;

type pluralFormatOptionsRe = {. "style": option(pluralStyle)};

type pluralFormatOptionsJs = {. "style": Js.nullable(string)};

let mapReasonPluralFormatOptionsToJs = options => {
  "style": options##style |> mapReasonPluralStyleToJs,
};

/* Components */
type domTag =
  | Span
  | Div
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | P
  | Strong
  | B
  | I
  | Em
  | Mark
  | Small
  | Del
  | Ins
  | Sub
  | Sup;

type textComponent =
  | DomTag(domTag)
  | ReactComponent(ReasonReact.reactClass);

let mapDomTagToString = tag =>
  switch (tag) {
  | Span => "span"
  | Div => "div"
  | P => "p"
  | H1 => "h1"
  | H2 => "h2"
  | H3 => "h3"
  | H4 => "h4"
  | H5 => "h5"
  | H6 => "h6"
  | Strong => "strong"
  | B => "b"
  | I => "i"
  | Em => "em"
  | Mark => "mark"
  | Small => "small"
  | Del => "del"
  | Ins => "ins"
  | Sub => "sub"
  | Sup => "sup"
  };

let mapOptDomTagToString = tag =>
  switch (tag) {
  | Some(tag) => Some(tag |> mapDomTagToString)
  | None => None
  };

let mapTextComponentToJs = textComponent =>
  switch (textComponent) {
  | DomTag(domTag) => mapDomTagToString(domTag)->Obj.magic
  | ReactComponent(reactComponent) => reactComponent->Obj.magic
  };

let mapOptTextComponentToJs = textComponent =>
  textComponent->Belt.Option.map(mapTextComponentToJs);

type errorHandler = string => unit;

module IntlProvider = {
  module Raw = {
    [@react.component] [@bs.module "react-intl"]
    external make:
      (
        ~locale: string=?,
        ~formats: Js.t({..})=?, /* TODO */
        ~messages: Js.Dict.t(string)=?,
        ~defaultLocale: string=?,
        ~defaultFormats: Js.t({..})=?, /* TODO */
        ~textComponent: textComponent=?,
        ~initialNow: int=?,
        ~onError: errorHandler=?,
        ~children: React.element
      ) =>
      React.element =
      "IntlProvider";
  };

  [@react.component]
  let make =
      (
        ~locale: option(string)=?,
        ~formats: option(Js.t({..}))=?, /* TODO */
        ~messages: option(Js.Dict.t(string))=?,
        ~defaultLocale: option(string)=?,
        ~defaultFormats: option(Js.t({..}))=?, /* TODO */
        ~textComponent: option(textComponent)=?,
        ~initialNow: option(int)=?,
        ~onError: option(errorHandler)=?,
        ~children,
        (),
      ) =>
    <Raw
      ?locale
      ?formats
      ?messages
      ?defaultLocale
      ?defaultFormats
      textComponent=?{textComponent->mapOptTextComponentToJs}
      ?initialNow
      ?onError>
      children
    </Raw>;
};

type intlJs('t) = {
  .
  "locale": string,
  "formats": Js.t('t), /* TODO */
  "messages": Js.Dict.t(string),
  "defaultLocale": string,
  "defaultFormats": Js.t('t), /* TODO */
  "formatDate":
    [@bs.meth] ((Js.Date.t, Js.nullable(dateTimeFormatOptionsJs)) => string),
  "formatTime":
    [@bs.meth] ((Js.Date.t, Js.nullable(dateTimeFormatOptionsJs)) => string),
  "formatRelative":
    [@bs.meth] ((Js.Date.t, Js.nullable(relativeFormatOptionsJs)) => string),
  "formatNumber":
    [@bs.meth] ((float, Js.nullable(numberFormatOptionsJs)) => string),
  "formatPlural":
    [@bs.meth] ((int, Js.nullable(pluralFormatOptionsJs)) => string),
  "formatMessage": [@bs.meth] ((message, Js.nullable(Js.t('t))) => string),
  "formatHTMLMessage":
    [@bs.meth] ((message, Js.nullable(Js.t('t))) => string),
  "now": [@bs.meth] (unit => int),
};

type intl('t) = {
  locale: string,
  formats: Js.t('t), /* TODO */
  messages: Js.Dict.t(string),
  defaultLocale: string,
  defaultFormats: Js.t('t), /* TODO */
  formatDate: Js.Date.t => string,
  formatDateWithOptions: (dateTimeFormatOptionsRe, Js.Date.t) => string,
  formatTime: Js.Date.t => string,
  formatTimeWithOptions: (dateTimeFormatOptionsRe, Js.Date.t) => string,
  formatRelative: Js.Date.t => string,
  formatRelativeWithOptions: (relativeFormatOptionsRe, Js.Date.t) => string,
  formatInt: int => string,
  formatIntWithOptions: (numberFormatOptionsRe, int) => string,
  formatFloat: float => string,
  formatFloatWithOptions: (numberFormatOptionsRe, float) => string,
  formatPlural: int => string,
  formatPluralWithOptions: (pluralFormatOptionsRe, int) => string,
  formatMessage: message => string,
  formatMessageWithValues: (Js.t('t), message) => string,
  formatHTMLMessage: message => string,
  formatHTMLMessageWithValues: (Js.t('t), message) => string,
  now: unit => int,
};

let mapIntlJsToReason = (intlJs: intlJs('a)): intl('a) => {
  locale: intlJs##locale,
  formats: intlJs##formats,
  messages: intlJs##messages,
  defaultLocale: intlJs##defaultLocale,
  defaultFormats: intlJs##defaultFormats,
  formatDate: value =>
    intlJs##formatDate(value, None |> Js.Nullable.fromOption),
  formatDateWithOptions: (options, value) =>
    intlJs##formatDate(
      value,
      Some(options |> mapReasonDateTimeFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatTime: value =>
    intlJs##formatTime(value, None |> Js.Nullable.fromOption),
  formatTimeWithOptions: (options, value) =>
    intlJs##formatTime(
      value,
      Some(options |> mapReasonDateTimeFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatRelative: value =>
    intlJs##formatRelative(value, None |> Js.Nullable.fromOption),
  formatRelativeWithOptions: (options, value) =>
    intlJs##formatRelative(
      value,
      Some(options |> mapReasonRelativeFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatInt: value =>
    intlJs##formatNumber(
      value |> float_of_int,
      None |> Js.Nullable.fromOption,
    ),
  formatIntWithOptions: (options, value) =>
    intlJs##formatNumber(
      value |> float_of_int,
      Some(options |> mapReasonNumberFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatFloat: value =>
    intlJs##formatNumber(value, None |> Js.Nullable.fromOption),
  formatFloatWithOptions: (options, value) =>
    intlJs##formatNumber(
      value,
      Some(options |> mapReasonNumberFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatPlural: value =>
    intlJs##formatPlural(value, None |> Js.Nullable.fromOption),
  formatPluralWithOptions: (options, value) =>
    intlJs##formatPlural(
      value,
      Some(options |> mapReasonPluralFormatOptionsToJs)
      |> Js.Nullable.fromOption,
    ),
  formatMessage: message =>
    intlJs##formatMessage(message, None |> Js.Nullable.fromOption),
  formatMessageWithValues: (values, message) =>
    intlJs##formatMessage(message, Some(values) |> Js.Nullable.fromOption),
  formatHTMLMessage: message =>
    intlJs##formatHTMLMessage(message, None |> Js.Nullable.fromOption),
  formatHTMLMessageWithValues: (values, message) =>
    intlJs##formatHTMLMessage(
      message,
      Some(values) |> Js.Nullable.fromOption,
    ),
  now: () => intlJs##now(),
};

module IntlInjector = {
  module Raw = {
    [@react.component] [@bs.module "./IntlInjector.js"]
    external make: (~children: intlJs('a) => React.element) => React.element =
      "IntlInjector";
  };

  [@react.component]
  let make = (~children, ()) =>
    <Raw> {intl => children(mapIntlJsToReason(intl))} </Raw>;
};

module FormattedMessage = {
  module Raw = {
    [@react.component] [@bs.module "react-intl"]
    external make:
      (
        ~id: string,
        ~defaultMessage: string,
        ~values: Js.t({..})=?,
        ~tagName: string=?
      ) =>
      React.element =
      "FormattedMessage";
  };

  [@react.component]
  let make =
      (
        ~id: string,
        ~defaultMessage: string,
        ~values: option(Js.t({..}))=?,
        ~tagName: option(domTag)=?,
        (),
      ) =>
    <Raw
      id
      defaultMessage
      ?values
      tagName=?{mapOptDomTagToString(tagName)}
    />;
};

/* DefinedMessage is another wrapper for FormattedMessage.
   It takes the id and defaultMessage props from a passed message object. */
module DefinedMessage = {
  [@react.component]
  let make =
      (
        ~message: message,
        ~values: option(Js.t({..}))=?,
        ~tagName: option(domTag)=?,
        (),
      ) =>
    <FormattedMessage.Raw
      id=message##id
      defaultMessage=message##defaultMessage
      ?values
      tagName=?{mapOptDomTagToString(tagName)}
    />;
};

module FormattedDate = {
  module Raw = {
    [@react.component] [@bs.module "react-intl"]
    external make:
      (
        ~value: Js.Date.t,
        ~format: string=?,
        ~localeMatcher: string=?,
        ~formatMatcher: string=?,
        ~timeZone: string=?,
        ~hour12: bool=?,
        ~weekday: string=?,
        ~era: string=?,
        ~year: string=?,
        ~month: string=?,
        ~day: string=?,
        ~hour: string=?,
        ~minute: string=?,
        ~second: string=?,
        ~timeZoneName: string=?
      ) =>
      React.element =
      "FormattedDate";
  };

  [@react.component]
  let make =
      (
        ~value: Js.Date.t,
        ~format: option(string)=?,
        ~localeMatcher: option(localeMatcher)=?,
        ~formatMatcher: option(formatMatcher)=?,
        ~timeZone: option(string)=?,
        ~hour12: option(bool)=?,
        ~weekday: option(textualFormat)=?,
        ~era: option(textualFormat)=?,
        ~year: option(numeralFormat)=?,
        ~month: option(mixedFormat)=?,
        ~day: option(numeralFormat)=?,
        ~hour: option(numeralFormat)=?,
        ~minute: option(numeralFormat)=?,
        ~second: option(numeralFormat)=?,
        ~timeZoneName: option(timeZoneName)=?,
        (),
      ) =>
    <Raw
      value
      ?format
      localeMatcher=?{localeMatcher |> mapReasonLocaleMatcherToJs}
      formatMatcher=?{formatMatcher |> mapReasonFormatMatcherToJs}
      ?timeZone
      ?hour12
      weekday=?{weekday |> mapReasonTextualFormatToJs}
      era=?{era |> mapReasonTextualFormatToJs}
      year=?{year |> mapReasonNumeralFormatToJs}
      month=?{month |> mapReasonMixedFormatToJs}
      day=?{day |> mapReasonNumeralFormatToJs}
      hour=?{hour |> mapReasonNumeralFormatToJs}
      minute=?{minute |> mapReasonNumeralFormatToJs}
      second=?{second |> mapReasonNumeralFormatToJs}
      timeZoneName=?{timeZoneName |> mapReasonTimeZoneNameToJs}
    />;
};

module FormattedTime = {
  module Raw = {
    [@react.component] [@bs.module "react-intl"]
    external make:
      (
        ~value: Js.Date.t,
        ~format: string=?,
        ~localeMatcher: string=?,
        ~formatMatcher: string=?,
        ~timeZone: string=?,
        ~hour12: bool=?,
        ~weekday: string=?,
        ~era: string=?,
        ~year: string=?,
        ~month: string=?,
        ~day: string=?,
        ~hour: string=?,
        ~minute: string=?,
        ~second: string=?,
        ~timeZoneName: string=?
      ) =>
      React.element =
      "FormattedTime";
  };

  [@react.component]
  let make =
      (
        ~value: Js.Date.t,
        ~format: option(string)=?,
        ~localeMatcher: option(localeMatcher)=?,
        ~formatMatcher: option(formatMatcher)=?,
        ~timeZone: option(string)=?,
        ~hour12: option(bool)=?,
        ~weekday: option(textualFormat)=?,
        ~era: option(textualFormat)=?,
        ~year: option(numeralFormat)=?,
        ~month: option(mixedFormat)=?,
        ~day: option(numeralFormat)=?,
        ~hour: option(numeralFormat)=?,
        ~minute: option(numeralFormat)=?,
        ~second: option(numeralFormat)=?,
        ~timeZoneName: option(timeZoneName)=?,
      ) =>
    <Raw
      value
      ?format
      localeMatcher=?{localeMatcher |> mapReasonLocaleMatcherToJs}
      formatMatcher=?{formatMatcher |> mapReasonFormatMatcherToJs}
      ?timeZone
      ?hour12
      weekday=?{weekday |> mapReasonTextualFormatToJs}
      era=?{era |> mapReasonTextualFormatToJs}
      year=?{year |> mapReasonNumeralFormatToJs}
      month=?{month |> mapReasonMixedFormatToJs}
      day=?{day |> mapReasonNumeralFormatToJs}
      hour=?{hour |> mapReasonNumeralFormatToJs}
      minute=?{minute |> mapReasonNumeralFormatToJs}
      second=?{second |> mapReasonNumeralFormatToJs}
      timeZoneName=?{timeZoneName |> mapReasonTimeZoneNameToJs}
    />;
};

/* Utils */
let wrapUnicodeString = (input: string) => {j|$input|j};

let wrapOptUnicodeString = (input: Js.nullable(string)) =>
  switch (input |> Js.Nullable.toOption) {
  | Some(input) => input |> wrapUnicodeString
  | None => ""
  };

let messagesArrayToDict = (translation: jsonMessages) =>
  translation->Array.reduce(
    Js.Dict.empty(),
    (dict, message) => {
      let unicodeMessage = message##message |> wrapOptUnicodeString;
      let unicodeDefaultMessage = message##defaultMessage |> wrapUnicodeString;
      Js.Dict.set(
        dict,
        message##id,
        unicodeMessage !== "" ? unicodeMessage : unicodeDefaultMessage,
      );
      dict;
    },
  );
