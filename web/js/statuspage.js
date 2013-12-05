<!--- admin -->
zh = 2;
mx = 0;
my = 0;
lastGood = null;
loaded = new Date();
hSession = "";
topp = 100;

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

function timeInfo()
{
  if (lastGood)
    return "<br/>Last Success: "+lastGood.toLocaleString()+" ("+duration(Date.now() - lastGood.getTime())+" ago)";
  else
    return "<br/>No successful status loads";
}
	
function spinit()
{
   jQuery(document).ready(function(){
   $(document).mousemove(function(e){
      mx = e.pageX;
	  my = e.pageY;
   }); 
   $(document).mousedown(function(e){
      var m = document.getElementById("pmenu");
      if (e.target.id.substr(0,5) != "pmenu")
	    m.style.display = "none";      
   }); 
  });
  $("#error").ajaxError( function(event, jqXHR, ajaxSettings, thrownError)
	  {
        $("#error").show();
        $("#error").html("Error contacting HL7Connect: "+jqXHR.statusText+timeInfo()+"<br/><a href=\"javascript:goflat()\">Go back to normal Status Page</a>");
	  }
	);

  setTimeout('loadStatus()', 10);
}

function loadStatus()
{
  queryStatus();
  setTimeout('loadStatus()', 5000);
}

function queryStatus()
{ 
  try
  {
     //jQuery.get("/xint/jstats", null, function(data) { process(data); });
    $.ajax({
      url: "/xint/jstats",
      cache: false,
      dataType: "json",
      success: function(data){
        process(data);
      },
      error: function(jqXHR, textStatus, errorThrown){
        $("#error").show();
		if (errorThrown)
          $("#error").html("System Error: "+errorThrown+timeInfo());
		else
          $("#error").html("System Error: "+textStatus+timeInfo());
      }
    });
  }
  catch (err)
  {
    $("#error").show();
    $("#error").html("System Error: "+err+timeInfo());
  }
  
};

function istart(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/int/startfeed?interfacekey="+key;
}

function istop(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/int/stopfeed?interfacekey="+key;
}

function iunschedule(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/int/unschedulefeed?interfacekey="+key;
}

function ireschedule(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/int/reschedulefeed?interfacekey="+key;
}

function route(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/routing.ksp?interfacekey="+key;
}

function sendc(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/sendcontrol.ksp?interfacekey="+key;
}

function editc(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/editinterface.ksp?interface="+key;
}

function histc(key)
{
  var m = document.getElementById("pmenu");
  m.style.display = "none";      
  window.location = "/feedaudit.ksp?interfacekey="+key;
}

function menu(key, is_in, status, schedule)
{
  var m = $("#pmenu");
  m.html("");
  
  
  if (status)
    m.append("<div class=\"popupitem\" id=\"pmenu-a\" onclick=\"istop("+key+")\">Stop</div>");
  else
    m.append("<div class=\"popupitem\" id=\"pmenu-a\" onclick=\"istart("+key+")\">Start</div>");
  if (schedule == "on")
    m.append("<div class=\"popupitem\" id=\"pmenu-a\" onclick=\"iunschedule("+key+")\">Turn scheduling Off</div>");
  else if (schedule == "off")
    m.append("<div class=\"popupitem\" id=\"pmenu-a\" onclick=\"ireschedule("+key+")\">Turn scheduling On</div>");

  if (is_in == true)
    m.append("<div class=\"popupitem\" id=\"pmenu-b\" onclick=\"route("+key+")\">Routing</div>");
  else
    m.append("<div class=\"popupitem\" id=\"pmenu-b\" onclick=\"sendc("+key+")\">Send Control</div>");
  
  m.append("<div class=\"popupitem\" id=\"pmenu-c\" onclick=\"editc("+key+")\">Configure</div>");
  m.append("<div class=\"popupitem\" id=\"pmenu-d\" onclick=\"histc("+key+")\">History</div>");

  m = document.getElementById("pmenu");
  m.style.position = "absolute";
  m.style.top = my+"px";
  m.style.left = mx+"px";
  m.style.zindex = zh+10;
  m.style.display = "block";
  
}

function moved(divn)
{
  var div = $(divn);
  zh++;
  div.css({'z-index':zh});
  localStorage.setItem("div-"+divn+"-position", div.css("position"));
  localStorage.setItem("div-"+divn+"-left", div.css("left"));
  localStorage.setItem("div-"+divn+"-top", div.css("top"));
  localStorage.setItem("div-"+divn+"-width", div.width());
  if (localStorage.getItem("div-"+divn+"-pin") != "1")
  {
    if (div.height() > 20)
      localStorage.setItem("div-"+divn+"-height", div.height());
  }
  localStorage.setItem("div-"+divn+"-display", div.css("display"));
  localStorage.setItem("div-"+divn+"-zindex", div.css("z-index"));
}

function hideme(divn)
{
  $(divn).hide();
  moved(divn);
}

function showme(divn)
{
  var d = $(divn);
  if (d.css("display") == 'none') 
  {
    d.show();
    moved(divn);
  }
  else
    hideme(divn);
}

function toggle(divn)
{
  var dn = divn.substr(1);
  if (localStorage.getItem("div-"+dn+"-pin") == "1")
    localStorage.setItem("div-"+dn+"-pin", "0");
  else
    localStorage.setItem("div-"+dn+"-pin", "1");
  $(divn).remove();
  queryStatus();
}

function reset()
{
  localStorage.clear();
  window.location.reload();
}

function goflat()
{
  setCookie("statuspage", "flat");
  window.location.reload();
}

<!-- page loader utilities -->

function addButton(id, name, status, cmd, big)
{
  var stf = $('#stfoot');
  stf.append("<span class=\"stfbreak\">&nbsp;</span>");

  if (big)
    stf.append("<span class=\"stfbreak\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span>");

  if (cmd)
    stf.append("<span class=\"stfcommand\" onClick=\""+cmd+"()\"><span class=\"ab"+status+"\">"+name+"</span></span>");
  else
    stf.append("<span class=\"stfitem\" onClick=\"showme('#g-"+id+"')\"><span class=\"ab"+status+"\">"+name+"</span></span>");
}

function loadGroupTable(group)
{
  var td = document.getElementById("title-"+group.id);
  td.className = "tackTitle"+group.status;
  var t = $("#gt-"+group.id);
  t.html("");
  if (group.in.length > 0)
  {
	t.append('<tr id="t"><td colspan="3" class="IncomingOutgoingHeading"><img src="images/Icons/Incoming.png" border="0"><b>&nbsp;Incoming&nbsp;</br></td><td class="IncomingOutgoingSubHeading">#</td><td class="IncomingOutgoingSubHeading">Errors</td><td class="IncomingOutgoingSubHeading">Last&nbsp;Use</td><td class="IncomingOutgoingSubHeading"></td><td class="IncomingOutgoingSubHeading">Status</td><td class="IncomingOutgoingSubHeading">Notes</td></tr>');
	for (i=0; i < group.in.length; i++)
	{
	  var g = group.in[i];
	  var s = '<tr id="tr-'+g.key+'" onclick="menu('+g.key+', true, '+g.started+', \''+g.schedule+'\')" style="cursor: pointer">';
	  if (g.hasScripts)
  	    s += '<td class="StatusPageTableLeftBorderColourScript" width=5px>&nbsp;</td><td class="StatusPageTableLeftPadding"></td>';
	  else
  	    s += '<td class="StatusPageTableLeftBorderColour" width=5px>&nbsp;</td><td class="StatusPageTableLeftPadding"></td>';
	  s += '<td class="stInterfaceName" title="'+g.type.escapeHTML()+'"><div style="height:0px;border:0px solid blue;padding:0px;margin:0px;"></div><a href="javascript:menu('+g.key+', true, '+g.started+', \''+g.schedule+'\')" class="NoUnderline">'+g.name.escapeHTML()+'</a></td>';
	  s += '<td class="InterfaceMsgNo">'+g.count+''+g.contentCode+'</td>';
	  if (g.errors)
	    s += '<td class="InterfaceErrors"><a class="InterfaceErrorsLink" href="/interfaceerrors.ksp?InterfaceKey="'+g.key+'">'+g.errors+'</a></td>';
	  else
	    s += '<td class="InterfaceErrors"></td>';
	  if (g.lastUse)
	    s += '<td class="InterfaceLastUse">'+g.lastUse+'</td>';
	  else
	    s += '<td class="InterfaceLastUse">--</td>';
	  s += '<td class="InterfaceWaiting"></td>';
	  if (g.started)
	  {
	    if (g.working == 'sleep')
	      s += '<td class="interface-status-sleeping">'+g.status.escapeHTML()+'</td>';
		else if (g.working)
	      s += '<td class="interface-status-going">'+g.status.escapeHTML()+'</td>';
	    else
  		  s += '<td class="interface-status-problem">'+g.status.escapeHTML()+'</td>';
	  }
	  else
	  {
	    if (!g.stopInfo)
	      s += '<td class="interface-status-stopped">Stopped</td>';
		else if (g.stopError)
	      s += '<td class="interface-status-error">Stopped ('+g.stopInfo.escapeHTML()+')</td>';
		else
	      s += '<td class="interface-status-stopped">Stopped ('+g.stopInfo.escapeHTML()+')</td>';
	  }
	  if (g.comment)
	    s += '<td class="InterfaceNotes">'+g.comment.escapeHTML()+'</td></tr>';
	  else
	    s += '<td class="InterfaceNotes"></td></tr>';	  
	  t.append(s);
	}
  }
  if (group.out.length > 0)
  {
    t.append('<tr id="t"><td colspan="3" class="IncomingOutgoingHeading"><img src="images/Icons/Outgoing.png" border="0"><b>&nbsp;Outgoing&nbsp;</br></td><td class="IncomingOutgoingSubHeading">#</td><td class="IncomingOutgoingSubHeading">Errors</td><td class="IncomingOutgoingSubHeading">Last&nbsp;Use</td><td class="IncomingOutgoingSubHeading">Waiting</td><td class="IncomingOutgoingSubHeading">Status</td><td class="IncomingOutgoingSubHeading"></td></tr>');
	for (i=0; i < group.out.length; i++)
	{
	  var g = group.out[i];
	  var s = '<tr  onclick="menu('+g.key+', true, '+g.started+', \''+g.schedule+'\')" style="cursor: pointer">';
	  if (g.hasScripts)
  	    s += '<td class="StatusPageTableLeftBorderColourScript" width=5px>&nbsp;</td><td class="StatusPageTableLeftPadding"></td>';
	  else
  	    s += '<td class="StatusPageTableLeftBorderColour" width=5px>&nbsp;</td><td class="StatusPageTableLeftPadding"></td>';
	  s += '<td class="stInterfaceName" title="'+g.type.escapeHTML()+'"><div style="height:0px;border:0px solid blue;padding:0px;margin:0px;"></div><a href="javascript:menu('+g.key+', false, '+g.started+', \''+g.schedule+'\')" class="NoUnderline">'+g.name.escapeHTML()+'</a></td>';
	  s += '<td class="InterfaceMsgNo">'+g.count+''+g.contentCode+'</td>';
	  if (g.errors)
	    s += '<td class="InterfaceErrors"><a class="InterfaceErrorsLink" href="/interfaceerrors.ksp?InterfaceKey="'+g.key+'">'+g.errors+'</a></td>';
	  else
	    s += '<td class="InterfaceErrors"></td>';
	  if (g.sending)
	    s += '<td class="InterfaceLastUse">Sending Now</td>';	  
	  else if (g.lastUse)
	    s += '<td class="InterfaceLastUse">'+g.lastUse+'</td>';
	  else
	    s += '<td class="InterfaceLastUse">--</td>';
	  s += '<td class="InterfaceWaiting">'+g.queue+'</td>';
	  if (g.started)
	  {
	    if (g.working)
	      s += '<td class="interface-status-going">'+g.status.escapeHTML()+'</td>';
	    else
  		  s += '<td class="interface-status-problem">'+g.status.escapeHTML()+'</td>';
	  }
	  else
	  {
	    if (!g.stopInfo)
	      s += '<td class="interface-status-stopped">Stopped</td>';
		else if (g.stopError)
	      s += '<td class="interface-status-error">Stopped ('+g.stopInfo.escapeHTML()+')</td>';
		else
	      s += '<td class="interface-status-stopped">Stopped ('+g.stopInfo.escapeHTML()+')</td>';
	  }
	  if (g.comment)
	    s += '<td class="InterfaceNotes">'+g.comment.escapeHTML()+'</td></tr>';
	  else
	    s += '<td class="InterfaceNotes"></td></tr>';	  
	  t.append(s);
	}
  }
}

function addGroup(group)
{
  zhl = zh;
  var d = document.getElementById("g-"+group.id);
  if (d == null)
  {
    d = document.createElement("div");
	d.id = "g-"+group.id;
	d.className = "ui-widget-contentsp";
	var d1 = document.createElement("div");
	d1.className = "sptack";
	var ex = localStorage.getItem("div-"+d.id+"-pin") != "1";
	if (!ex && (group.status == "Error" || group.status == "SomeError"))
	{
	  ex = true;
	  im = "pine";
	}
	else if (ex)
	  im = "pin0";
	else 
	  im = "pin1";
	d1.innerHTML = "<table cellspacing=0 cellpadding=0 border=0 width=100%><tr><td align=left><img id=\"g-"+group.id+"-t\" style='cursor: pointer' onclick='toggle(\"#g-"+group.id+"\")' src='/images/other/"+im+".png'/></td><td class=\"tackTitleError\" id=\"title-"+group.id+"\" align=center>"+group.name+"</td><td align=right><img style='cursor: pointer' onclick='hideme(\"#g-"+group.id+"\")' src='/images/other/close.png'/></td></tr></table>";
	d.appendChild(d1);
	var dt = document.createElement("table");
	d.appendChild(dt);
	dt.border = 0;
	dt.id = "gt-"+group.id;
	dt.className = "status-page-table";
	dt.cellspacing = 0;
	$("#gt-"+group.id).css({"padding-left":"20px"});
	dt.width="100%";
	document.getElementById("body").appendChild(d);
    if (ex)
	{
	  $("#g-"+group.id).resizable({
	    stop: function() {
				moved("#g-"+group.id);
		}
	  });
	};
	$("#g-"+group.id).draggable({
	  stop: function() {
				moved("#g-"+group.id);
			}
		});
	if (localStorage.getItem("div-#"+d.id+"-left"))
	{
      if (ex)
   	    $("#g-"+group.id).css({position:localStorage.getItem("div-#"+d.id+"-position"),top:localStorage.getItem("div-#"+d.id+"-top"),left:localStorage.getItem("div-#"+d.id+"-left"),width:localStorage.getItem("div-#"+d.id+"-width"),height:localStorage.getItem("div-#"+d.id+"-height")});
	  else
   	    $("#g-"+group.id).css({position:localStorage.getItem("div-#"+d.id+"-position"),top:localStorage.getItem("div-#"+d.id+"-top"),left:localStorage.getItem("div-#"+d.id+"-left"),width:localStorage.getItem("div-#"+d.id+"-width"),height:17});
    }
	else
	{
  	  $("#g-"+group.id).css({position:"absolute",top:topp});
	  topp = topp + 100;
  	  moved("#g-"+group.id);
	}
    if (localStorage.getItem("div-#"+d.id+"-display"))
	  $("#g-"+group.id).css({display:localStorage.getItem("div-#"+d.id+"-display")});
    if (localStorage.getItem("div-#"+d.id+"-zindex"))
	{
	  $("#g-"+group.id).css({'z-index':localStorage.getItem("div-#"+d.id+"-zindex")});
	  if (localStorage.getItem("div-#"+d.id+"-zindex") > zhl)
	    zhl = localStorage.getItem("div-#"+d.id+"-zindex");
	}
	else
	{
	  $("#g-"+group.id).css({'z-index':zh});
	}
  };
  loadGroupTable(group);
  if (zhl > zh)
    zh = zhl; 
}

<!--- page loading control -->

function process(data)
{
  try 
  {
	lastGood = new Date();  
	  
    var stf = $('#stfoot');
	stf.html("");
   
    topp = 100;
	var o = data; // JSON.parse(data);
	
    if (hSession && (hSession != o.Session))
	{
      window.location.reload();
	  return;
	}
    hSession = o.Session;
	
    document.getElementById("gwstats").innerHTML = o.Summary;
	for (var i = 0; i < o.groups.length; i++) { 
      addButton(o.groups[i].id, o.groups[i].name, o.groups[i].status);
	  addGroup(o.groups[i]);
    }
	addButton("0", "Flat", "NotGoing", "goflat", true);
	addButton("0", "Reset", "NotGoing", "reset");
    $("#error").hide();
  }
  catch (error) {
    $("#error").show();
    $("#error").html("System Error: "+error+timeInfo());
  }
}


	  
