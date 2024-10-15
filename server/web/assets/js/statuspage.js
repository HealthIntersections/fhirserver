<!--- admin -->

String.prototype.escapeHTML = function () {                                       
        return(                                                                 
            this.replace(/&/g,'&amp;').                                         
                replace(/>/g,'&gt;').                                           
                replace(/</g,'&lt;').                                           
                replace(/"/g,'&quot;')                                         
        );                                                                     
    };

function setCookie(c_name,value,exdays)
{
var exdate=new Date();
exdate.setDate(exdate.getDate() + exdays);
var c_value=escape(value) + ((exdays==null) ? "" : "; expires="+exdate.toUTCString());
document.cookie=c_name + "=" + c_value;
}

function getCookie(c_name)
{
var i,x,y,ARRcookies=document.cookie.split(";");
for (i=0;i<ARRcookies.length;i++)
{
  x=ARRcookies[i].substr(0,ARRcookies[i].indexOf("="));
  y=ARRcookies[i].substr(ARRcookies[i].indexOf("=")+1);
  x=x.replace(/^\s+|\s+$/g,"");
  if (x==c_name)
    {
    return unescape(y);
    }
  }
}
	
function duration(ms)
{
  var x = ms / 1000;
  days = Math.floor(x / (24 * 60 * 60));
  x = x - (days * 24 * 60 * 60);
  hours = Math.floor(x / (60 * 60));
  x = x - (hours * 60 * 60);
  minutes = Math.floor(x / (60));
  x = x - (minutes * 60);
  seconds = Math.floor(x);
  
  if (days)
  {
    if (hours && days < 3)
	  return days+" days "+hours+" hours"; 
	else
	  return days+" days"; 
  }
  else if (hours)
  {
    if (minutes && hours < 3)
	  return hours+" hours "+minutes+" min"; 
	else
	  return hours+" hours"; 
  }
  else if (minutes)
  {
    if (seconds && minutes < 5)
	  return minutes+" min "+seconds+" sec"; 
	else
	  return minutes+" min"; 
  }
  else
    return String(seconds)+" secs";
}

