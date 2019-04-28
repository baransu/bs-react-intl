const React = require("react");
const ReactIntl = require("react-intl");

const IntlInjector = ReactIntl.injectIntl(
  class extends React.Component {
    render() {
      return this.props.children(this.props.intl);
    }
  }
);

exports.IntlInjector = IntlInjector;
