#!/usr/bin/env node

// TODO: This only works for GitLab. Add support for GitHub.
const assert = require('assert');

const url = process.argv[2];
const branch = process.argv[3];
assert(url, 'No git remote URL was provided.');
assert(branch, 'No branch name was given.');

function processUrl(url) {
  // https://domain/user/repo.git
  if (/^https?:\/\//.test(url)) {
    return url.replace(/.git$/, '');
  }

  // git@sub.domain.tld:user/repo.git
  if (/^git@/.test(url)) {
    return url
      .replace(/:(.+).git$/, '/$1')
      .replace(/^git@/, 'https://');
  }
}

function processMrQuery(branch) {
  const prefix = encodeURIComponent('merge_request[source_branch]');
  const escapedBranch = encodeURIComponent(branch).replace(/prod/, 'pr0d');
  return `${prefix}=${escapedBranch}`;
}

const processedUrl = processUrl(url);
const query = processMrQuery(branch);
if (processedUrl && query) {
  const result = `${processedUrl}/merge_requests/new?${query}`;
  process.stdout.write(result);
} else {
  process.exit(1);
}
