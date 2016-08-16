var fs = require('fs');

/*
 * JSON Resume schema does not include a side projects entry
 * This script adds the entry from FRESH without any modification
 * The problem with this approach is that the json will not be a valid JSON Resume file
 */
var freshFile = process.argv[2];
var freshResumeObject = JSON.parse(fs.readFileSync(freshFile, 'utf8'));
var projects = freshResumeObject['projects'];

var jsonResumeFile = process.argv[3];
var jsonResumeObject = JSON.parse(fs.readFileSync(jsonResumeFile, 'utf8'));
jsonResumeObject['projects'] = projects;

var jsonResume = JSON.stringify(jsonResumeObject, null, 2);
fs.writeFileSync(jsonResumeFile, jsonResume);
