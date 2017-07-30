#!/usr/bin/env node
const { join } = require('path');
const dir = process.cwd();

let json;
try {
  json = require(join(dir, 'package.json'));
} catch (error) {
  console.error('No package.json file.');
  process.exit(1);
}

const { scripts = {} } = json;

if ('test:watch' in scripts) {
  process.stdout.write('test:watch');
} else if ('test' in scripts) {
  process.stdout.write('test -- --watch');
} else {
  console.error('No test script found.');
  process.exit(1);
}
