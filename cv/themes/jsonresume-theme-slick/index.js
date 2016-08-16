var fs = require("fs");
var Handlebars = require("handlebars");

Handlebars.registerHelper("formatDate", function(date) {
  var dateList =  date.split("-");
  if(dateList.length > 1) {
    return dateList[1] + "-" + dateList[0];
  }
  else {
    return date;
  }
});

function render(resume) {
	if (resume.basics && resume.basics.profiles.length > 0) {
		for (var i=0; i < resume.basics.profiles.length; i++) {
			resume.basics.profiles[i].class = resume.basics.profiles[i].network.toLowerCase();
		}
	}

	var css = fs.readFileSync(__dirname + "/style.css", "utf-8");
	var template = fs.readFileSync(__dirname + "/resume.template", "utf-8");
	return Handlebars.compile(template)({
		css: css,
		resume: resume
	});
}

module.exports = {
	render: render
};
