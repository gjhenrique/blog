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
                  {name: "Red", stations: ["D", "B", "E"]}];

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
                  {name: "Red", stations: ["D", "B", "E"]}];

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

  var containers6 = {
    graph_container: "alg-7",
    git_container: "alg-7-git",
    current_node: "B",
    traversal_function: 10,
  };
  var config12 = [{name: "Blue", stations: ["A", "B", "C"]}];

  var graph6 = metro.animations.build_animation(containers6, config12)

  document.getElementById("alg-7-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph6);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph6);
    }
  });

  var containers7 = {
    graph_container: "alg-8",
    // git_container: "alg-8-git",
    current_node: "B",
    traversal_function: 10,
  };
  var config13 = [{name: "Blue", stations: ["A", "B"]},
                  {name: "Red", stations: ["B", "C"]}];

  var graph7 = metro.animations.build_animation(containers7, config13)

  document.getElementById("alg-8-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph7);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph7);
    }
  });

  var containers8 = {
    graph_container: "alg-9",
    git_container: "alg-9-git",
    current_node: "B",
    traversal_function: 10,
  };

  var config14 = [{name: "Blue", stations: ["A", "B"]},
                  {name: "Red", stations: ["C", "B"]}];

  var graph8 = metro.animations.build_animation(containers8, config14)

  document.getElementById("alg-9-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph8);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph8);
    }
  });

  var containers9 = {
    graph_container: "alg-10",
    // git_container: "alg-10-git",
    current_node: "B",
    traversal_function: 10,
  };

  var config15 = [{name: "Blue", stations: ["A", "B", "D"]},
                  {name: "Red", stations: ["C", "B", "D"]}];

  var graph9 = metro.animations.build_animation(containers9, config15)

  document.getElementById("alg-10-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph9);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph9);
    }
  });

  var containers10 = {
    graph_container: "alg-11",
    git_container: "alg-11-git",
    current_node: "B",
    traversal_function: 10,
  };

  var config16 = [{name: "Green", stations: ["A", "D", "E"]},
                  {name: "Red", stations: ["B", "D", "F", "G"]},
                  {name: "Blue", stations: ["C", "D", "F", "H"]}];

  var graph10 = metro.animations.build_animation(containers10, config16)

  document.getElementById("alg-11-button").addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph10);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph10);
    }
  });
}, false);
