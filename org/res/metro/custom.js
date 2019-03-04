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

  var config4 = [{name: "Red", stations: ["A", "B", "C", "D", "A"]}];
  var container4 = {
    graph_container: "build-4",
  };
  metro.animations.build_raw_animation(container4, config4, {git_container: false})

  var config5 = [{name: "Red", stations: ["A", "B", "C", "D"]}];
  var container5 = {
    graph_container: "build-5",
  };
  metro.animations.build_raw_animation(container5, config5, {git_container: false})

  var config6 = [{name: "Green", stations: ["A", "B", "C"]}];
  var container6 = {
    graph_container: "alg-1",
  };
  metro.animations.build_raw_animation(container6, config6, {git_container: false})

  var containers1 = {
    graph_container: "alg-2",
    current_node: "B",
    traversal_function: 1,
  };

  var config7 = [{name: "Green", stations: ["A", "B", "C"]}];

  var graph1 = metro.animations.build_animation(containers1, config7)

  document.getElementById("alg-2-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph1);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph1);
    }
  });

  var containers2 = {
    graph_container: "alg-3",
    current_node: "B",
    traversal_function: 2,
  };

  var config8 = [{name: "Green", stations: ["A", "B", "C"]}];

  var graph2 = metro.animations.build_animation(containers2, config8)

  document.getElementById("alg-3-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph2);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph2);
    }
  });

  var containers3 = {
    graph_container: "alg-4",
    current_node: "B",
    traversal_function: 3,
  };

  var config9 = [{name: "Green", stations: ["A", "B", "C"]}];

  var graph3 = metro.animations.build_animation(containers3, config9)

  document.getElementById("alg-4-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph3);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph3);
    }
  });

  var containers4 = {
    graph_container: "alg-5",
    current_node: "B",
    traversal_function: 3,
  };

  var config10 = [{name: "Green", stations: ["A", "B", "C"]},
                  {name: "Blue", stations: ["D", "B", "E"]}];

  var graph4 = metro.animations.build_animation(containers4, config10)

  document.getElementById("alg-5-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph4);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph4);
    }
  });

  var containers5 = {
    graph_container: "alg-6",
    current_node: "B",
    traversal_function: 10,
  };

  var config11 = [{name: "Green", stations: ["A", "B", "C"]},
                  {name: "Blue", stations: ["D", "B", "E"]}];

  var graph5 = metro.animations.build_animation(containers5, config11)

  document.getElementById("alg-6-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph5);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph5);
    }
  });
}, false);
