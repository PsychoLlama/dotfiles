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

if (typeof scripts.test !== 'string' || /no test specified/.test(scripts.test)) {
  console.error('This project has no tests.');
  process.exit(1);
}

const testScript = scripts.test;

if (/jest/.test(testScript)) {
  process.stdout.write('jest -- --watch --collectCoverage=false');
  process.exit();
}

if (/mocha/.test(testScript)) {
  process.stdout.write('test -- --watch --reporter min');
  process.exit();
}

// I don't care enough about other testing frameworks.
process.stdout.write('test -- --watch');
