// initialize an introjs instance          
var intro = introJs();

// handler 1
Shiny.addCustomMessageHandler("setHelpContent",
  
  // callback function. 
  // note: message is passed by shiny and contains the tour data
  function(message){

    // load data 
    intro.setOptions({steps: message.steps });
    
  }
  
);

// handler 2
Shiny.addCustomMessageHandler("startHelp",
  
  // callback function
  function(message) {

    // start intro.js
    // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

// handler 1
Shiny.addCustomMessageHandler("setHelpContent2",
  
  // callback function. 
  // note: message is passed by shiny and contains the tour data
  function(message){

    // load data 
    intro.setOptions({steps: message.steps });
    
  }
  
);

// handler 2
Shiny.addCustomMessageHandler("startHelp2",
  
  // callback function
  function(message) {

    // start intro.js
    // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);

// handler 1
Shiny.addCustomMessageHandler("setHelpContent3",
  
  // callback function. 
  // note: message is passed by shiny and contains the tour data
  function(message){

    // load data 
    intro.setOptions({steps: message.steps });
    
  }
  
);

// handler 2
Shiny.addCustomMessageHandler("startHelp3",
  
  // callback function
  function(message) {

    // start intro.js
    // note: we don't need information from shiny, just start introJS
    intro.start();
  }
  
);
