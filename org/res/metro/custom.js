document.addEventListener('DOMContentLoaded', function(){
  var config1 = [{name: "Red", stations: ["A", "C", "E"]},
                   {name: "Blue", stations: ["B", "C", "F"]}];

  var container1 = {
    graph_container: "build-1",
  };
  metro.animations.build_raw_animation(container1, config1, {git_container: false})

  var config2 = [{name: "Red", stations: ["B", "C", "D"]},
                 {name: "Blue", stations: ["D", "B", "A"]}];

  var container2 = {
    graph_container: "build-2",
  };
  metro.animations.build_raw_animation(container2, config2, {git_container: false})

  var config3 = [{name: "Red", stations: ["B", "C", "D"]},
                 {name: "Blue", stations: ["A", "B", "D"]}];
  var container3 = {
    graph_container: "build-3",
  };
  metro.animations.build_raw_animation(container3, config3, {git_container: false})

  var containers = {
    graph_container: "graph-simple",
    git_container: "commands-simple",
    current_node: "C",
    traversal_function: 5,
    // timeout: 500
  };

  var configSimple = [{name: "Red", stations: ["A", "B", "C"]},
                      {name: "Blue", stations: ["D", "B", "E"]},
                      {name: "Green", stations: ["F", "B", "G"]}];

  var state1 = metro.animations.build_animation(containers, configSimple)

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
