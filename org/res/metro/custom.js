function buildRawAnimation(containerConfig, config) {
  metro.animations.build_raw_animation(containerConfig, config, {git_container: false})
}

function buildAnimation(containerConfig, config, buttonId) {
  var graph = metro.animations.build_animation(containerConfig, config)

  document.getElementById(buttonId).addEventListener("click", function(event) {
    var element = event.target;
    if(element.classList.contains('fa-play')) {
      element.classList.remove('fa-play');
      element.classList.add('fa-pause');
      metro.animations.start_animation(graph);
    } else {
      element.classList.remove('fa-pause');
      element.classList.add('fa-play');
      metro.animations.stop_animation(graph);
    }
  });
}

document.addEventListener('DOMContentLoaded', function(){
  buildRawAnimation(
    {graph_container: "build-1"},
    [{name: "Red", stations: ["A", "C", "E"]},
     {name: "Blue", stations: ["B", "C", "F"]}]
  );

  buildRawAnimation(
    {graph_container: "build-2", layout: "grid"},
    [{name: "Red", stations: ["B", "C", "D"]},
     {name: "Blue", stations: ["D", "B", "A"]}]
  );

  buildRawAnimation(
    {graph_container: "build-3"},
    [{name: "Red", stations: ["B", "C", "D"]},
     {name: "Blue", stations: ["A", "B", "D"]}]
  );

  buildRawAnimation(
    {graph_container: "build-4", layout: "grid"},
    [{name: "Red", stations: ["A", "B", "C", "D", "A"]}]
  );

  buildRawAnimation(
    {graph_container: "build-5"},
    [{name: "Red", stations: ["A", "B", "C", "D"]}]
  );

  buildRawAnimation(
    {graph_container: "alg-1"},
    [{name: "Green", stations: ["A", "B", "C"]}]
  );

  buildAnimation(
    {
      graph_container: "alg-2",
      current_node: "B",
      traversal_function: 1,
    },
    [{name: "Green", stations: ["A", "B", "C"]}],
    "alg-2-button"
  );

  buildAnimation(
    {
      graph_container: "alg-3",
      current_node: "B",
      traversal_function: 2,
    } ,
    [{name: "Green", stations: ["A", "B", "C"]}],
    "alg-3-button"
  );

  buildAnimation(
    {
      graph_container: "alg-4",
      current_node: "B",
      traversal_function: 3,
    },
    [{name: "Green", stations: ["A", "B", "C"]}],
    "alg-4-button"
  );

  buildAnimation(
    {
      graph_container: "alg-5",
      current_node: "B",
      traversal_function: 3,
    },
    [{name: "Green", stations: ["A", "B", "C"]},
     {name: "Red", stations: ["D", "B", "E"]}],
    "alg-5-button"
  );

  buildAnimation(
    {
      graph_container: "alg-6",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Green", stations: ["A", "B", "C"]},
     {name: "Red", stations: ["D", "B", "E"]}],
    "alg-6-button"
  );

  buildAnimation(
    {
      graph_container: "alg-7",
      git_container: "alg-7-git",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Blue", stations: ["A", "B", "C"]}],
    "alg-7-button"
  );

  buildAnimation(
    {
      graph_container: "alg-8",
      git_container: "alg-8-git",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Blue", stations: ["A", "B"]},
     {name: "Red", stations: ["B", "C"]}],
    "alg-8-button"
  );

  buildAnimation(
    {
      graph_container: "alg-9",
      git_container: "alg-9-git",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Blue", stations: ["A", "B"]},
     {name: "Red", stations: ["C", "B"]}],
    "alg-9-button"
  );

  buildAnimation(
    {
      graph_container: "alg-10",
      git_container: "alg-10-git",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Blue", stations: ["A", "B", "D"]},
     {name: "Red", stations: ["C", "B", "D"]}],
    "alg-10-button"
  );

  buildAnimation(
    {
      graph_container: "alg-11",
      git_container: "alg-11-git",
      current_node: "B",
      traversal_function: 10,
    },
    [{name: "Green", stations: ["A", "D", "E"]},
     {name: "Red", stations: ["B", "D", "F", "G"]},
     {name: "Blue", stations: ["C", "D", "F", "H"]}],
    "alg-11-button"
  );
}, false);
