var argv = require('argv');

argv.option({
	name: 'output',
	short: 'o',
	type : 'string',
	description : 'Output file (default is `dep-check.html`)',
	example: "'elm-dep-check --output=report.html' or 'elm-dep-check -o report.html"
});

var args = argv.run();

require('./index.js').report(args.options);
