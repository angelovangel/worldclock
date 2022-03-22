
const timezone = Intl.DateTimeFormat().resolvedOptions().timeZone;
const date = new Date();
const offset = date.getTimezoneOffset();

$(document).on("shiny:connected", function() {
  // your awesome JavaScript here can use Shiny.setInputValue
  Shiny.setInputValue('client_timezone', timezone);
  Shiny.setInputValue('client_offset', offset);
});


//console.log(timezone); // Asia/Karachi