document.addEventListener('DOMContentLoaded', function(){
  var configSimple = [{name: "Red", stations: ["A", "B", "C"]},
                      {name: "Blue", stations: ["D", "B", "E"]},
                      {name: "Green", stations: ["F", "B", "G"]}];
  var containers = {
    graph_container: "graph-simple",
    git_container: "commands-simple",
    timeout: 500
  };

  var state1 = metro.animations.build_animation(containers, configSimple)
  metro.animations.stop_animation(state1);

  document.getElementById("metro-play-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(state1);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(state1);
    }
  });
}, false);
