#!/usr/bin/env node
const {join} = require('path');

let json;
try {
  const cwd = process.cwd();
  const filePath = join(cwd, 'package.json');
  json = require(filePath);
} catch (e) {
  console.warn('No package.json file.');
  process.exit(1);
}

const {scripts = {}} = json;

const getRunner = script => {
  // Trims simple environment variables (like NODE_ENV=test).
  const sanitized = script.replace(/^([A-Z_]+=\w+ )+/g, '');
  const [runner] = sanitized.split(' ');

  return runner;
};

const supportedScripts = ['test:unit', 'test'];
const hasScript = supportedScripts.some(script => {
  if (!scripts.hasOwnProperty(script)) return;

  const runner = getRunner(scripts[script]);
  const pair = `${script}\t${runner}`;
  process.stdout.write(pair);

  return true;
});

if (!hasScript) {
  console.error(`Ain't no runner available.`);
  process.exit(1);
}
