const queryString = window.location.search;
const urlParams = new URLSearchParams(queryString);
const demoMode = !(urlParams.get('demoMode') == undefined)

var between0and100 = function (input)
{
    try
    {
        parseInt(input);
    }
    catch(NumberFormatException)
    {
        return false;
    }
    if(100 >= parseInt(input) && parseInt(input) >= 0 ) {
      return true;
    } else {
      return false;
    }
}

function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.trial = slide({
    name : "trial",
    present: exp.all_stims,

    // PRESENT THE SLIDE
    present_handle: function(stim) {
      this.trial_start = new Date();
      this.stim = stim;
      this.item = stim.item;
      this.version = stim.version;
      this.center_embedding = stim.center_embedding; // added for extension
      this.passive = stim.passive;                   // added for extension
      this.header = stim.header;
      this.continuation = stim.continuation;
      this.title = stim.Title;

      $("#vignette").html(this.header + "<p>" + this.continuation);
      $("#question").html('<i>1. Do you think that the claim is covered under ' + this.item + ' as it appears in the policy?</i>');
      $("#error_percept").hide();
      $("#error_num").hide();
      if(!demoMode) {
        $("#demoView").hide();
      } else {
        $("#demoName").html("<b>Item name</b>: " + stim.Title);
        $("#demoCondition").html("<b>Item condition</b>: " + this.version);
      }

    },

    button_demo : function() {
      _stream.apply(this);
    },

    // CHECK THAT THEY MOVED ALL SLIDERS
    button_percept : function() {
    this.individual_judgment = $('input[name="individual_judgment"]:checked').val()
    this.population_judgment = $("#population_judgment").val()
    this.confidence = $("#confidence").val()
    verifyPopJudgment = between0and100(this.population_judgment)
    questions1or3NotAnswered = (this.individual_judgment === undefined || $("#confidence").val() == -1)
    if(!verifyPopJudgment && questions1or3NotAnswered) {
      $("#error_num").show();
      $("#error_percept").show();
    } else if (!verifyPopJudgment) {
      $("#error_num").show();
      $("#error_percept").hide();
    } else if (questions1or3NotAnswered) {
      $("#error_num").hide();
      $("#error_percept").show();
    } else {
      $("#error_num").hide();
      $("#error_percept").hide();
      this.log_responses();
      $('input:radio[name="individual_judgment"]:checked')[0].checked = false;      
      document.getElementById('population_judgment').value = '';
      document.getElementById('confidence').value = -1;
      _stream.apply(this);
    }
  },

    log_responses : function() {

      exp.data_trials.push({
          "individual_judgment" : this.individual_judgment,
          "population_judgment" : parseInt(this.population_judgment),
          "confidence" : this.confidence,
          "item" : this.item,
          "title" : this.title,
          "version" : this.version,
          "center_embedding": this.center_embedding, //added for extension
          "passive": this.passive,                    //added for extension
          "header" : this.header,
          "continuation" : this.continuation,
          "time": (new Date()) - this.trial_start,
          "slide_number_in_experiment" : exp.phase,
          "response_accent": exp.sliderPost_accent,
          "response_understand": exp.sliderPost_understand,
        });
    }

  });

slides.subj_info =  slide({
    name : "subj_info",
    button_submit : function(e){
      var raceData = new Array();
      var raceQs = document.getElementById("checkboxes");
      var chks = raceQs.getElementsByTagName("INPUT");
      for (var i = 0; i < chks.length; i++) {
        if (chks[i].checked) {
          raceData.push(chks[i].value);
        }
      };
      
      if ($("#participant_id").val() == 0) {
        $("#error_emptyid").show();
      } else {
      exp.participant_id = $("#participant_id").val();
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        affiliation : $("#affiliation").val(),
        race : raceData.join(", "),
        legaltraining : $("#legaltraining").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go();
      }
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "system" : exp.system,
          "hit_information" : exp.hit_data,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000,
          "participant_id" : exp.participant_id
      };
      proliferate.submit(exp.data);
    }
  });

  return slides;
}


/// init ///
function init() {

  exp.data_trials = [];

  stim_item_list = _.shuffle(_.uniq(_.map(stimuli, function(stim) { return stim.item })))
  console.log(stim_item_list)

  // 12 indices total
  stim_index = [{version: "uncovered", center_embedding: "yes", passive: "yes", item: stim_item_list[0]},
                {version: "uncovered", center_embedding: "yes", passive: "no", item: stim_item_list[1]},
                {version: "uncovered", center_embedding: "no", passive: "no", item: stim_item_list[2]},
                {version: "uncovered", center_embedding: "no", passive: "yes", item: stim_item_list[3]},
                {version: "covered", center_embedding: "yes", passive: "yes", item: stim_item_list[4]},
                {version: "covered", center_embedding: "yes", passive: "no", item: stim_item_list[5]},
                {version: "covered", center_embedding: "no", passive: "no", item: stim_item_list[6]},
                {version: "covered", center_embedding: "no", passive: "yes", item: stim_item_list[7]},
                {version: "controversial", center_embedding: "yes", passive: "yes", item: stim_item_list[8]},
                {version: "controversial", center_embedding: "yes", passive: "no", item: stim_item_list[9]},
                {version: "controversial", center_embedding: "no", passive: "no", item: stim_item_list[10]},
                {version: "controversial", center_embedding: "no", passive: "yes", item: stim_item_list[11]}]

  stim_list = _.map(_.range(0,stim_index.length), function(i){
    let index = stim_index[i]
    stim_retrieved = _.filter(stimuli, function(stim){
    return (stim.version == index.version && stim.center_embedding == index.center_embedding && stim.passive == index.passive && stim.item == index.item)})[0]
    return (stim_retrieved)
  })

  stims = demoMode ? stimuli: _.shuffle(stim_list)

  exp.all_stims = stims;

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "trial", "subj_info", "thanks"];

  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  $("#audio_player").bind("ended", function () {
        // if (! $("#attention_check").data("dont-show")) {
          // $("#attention_check").show();

        // }
        $("#audio_player").data("num-plays", $("#audio_player").data("num-plays") + 1);

      });

  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
