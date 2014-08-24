dlgsrc = null;

// Modern Combo Box  Script
// copyright Stephen Chapman, 18th October 2008
// you may copy this script provided that you retain the copyright notice

function combo(id,h,l) 
{
  var self = this; 
  self.h = h; 
  self.l = l; 
  self.inp = document.getElementById(id); 
  self.hasfocus = false; 
  self.sel = -1; 
  self.ul = self.inp.nextSibling; 
  while (self.ul.nodeType == 3) 
    self.ul = self.ul.nextSibling; 
  self.ul.onmouseover = function() {
    self.ul.className = '';
  }; 
  self.ul.onmouseout = function() {
    self.ul.className = 'focused'; 
    if (!self.hasfocus) 
      self.ul.style.display = 'none';
  }; 
  self.list = self.ul.getElementsByTagName('li'); 
  for (var i=self.list.length - 1; i >= 0; i--) {
    self.list[i].onclick = function() {
      self.inp.value = this.firstChild.data;  
      d = document.getElementById("fhir-tag-text");
      for (var i = 0; i < codes.length; i++) { 
        if (codes[i].system+"||"+codes[i].code == self.inp.id)
          d.value = codes[i].system+"||"+codes[i].code;
      }        
      self.rset(self);
    }
  } 
  self.inp.onfocus = function() {
    self.ul.style.display = 'block'; 
    self.ul.className = 'focused'; 
    self.hasfocus = true; 
    self.sel = -1;
  }; 
  self.inp.onblur = function() {
    if (self.ul.className=='focused') {
      self.rset(self);
    } 
    self.ul.className = ''; 
    self.hasfocus = false;
  }; 
  self.inp.onkeyup = function(e) {
    var k = (e)? e.keyCode : event.keyCode; 
    if (k == 40 || k == 13) {
      if (self.sel == self.list.length-1) {
        self.list[self.sel].style.backgroundColor = self.l; 
        self.sel = -1;
      } 
      if (self.sel > -1)
        self.list[self.sel].style.backgroundColor = self.l; 
      self.inp.value = self.list[++self.sel].firstChild.data; 
      self.list[self.sel].style.backgroundColor = self.h;
    } 
    else if (k == 38 && self.sel > 0) {
      self.list[self.sel].style.backgroundColor = self.l; 
      self.inp.value = self.list[--self.sel].firstChild.data; 
      self.list[self.sel].style.backgroundColor = self.h;
    }
    return false;
  };
} 
  
  combo.prototype.rset = function(self) {
    self.ul.style.display = 'none'; 
    self.sel = -1; 
    for (var i=self.list.length - 1; i >= 0; i--) {
      self.list[i].style.backgroundColor = self.l;
    }  
    return false;
  };

var tags = null;
var tgt = null;
var tags_tag = "";
var tags_profile = "";
var tags_security = "";

function commitTag()
{
  term = document.getElementById("fhir-tag-input").value;
  label = document.getElementById("fhir-tag-text").value;
  if (document.getElementById("fhir-tag-tt").checked) 
    scheme = "http://hl7.org/fhir/tag";
  else if (document.getElementById("fhir-tag-tp").checked)
    scheme = "http://hl7.org/fhir/tag/profile";
  else
    scheme = "http://hl7.org/fhir/tag/security";
  
  if (!term)
    alert("no URI - a URI must be provided");
  else if (term.match("^[^\s]+([\s]+[^\s]+)*$")) {
    if (!label)
      json = "{ \"resourceType\" : \"TagList\", \"category\" : [{ \"term\" : \""+term+"\", \"scheme\" : \""+scheme+"\" }] }";
    else
      json = "{ \"resourceType\" : \"TagList\", \"category\" : [{ \"term\" : \""+term+"\", \"label\" : \""+label+"\", \"scheme\" : \""+scheme+"\" }] }";
    sendTag(json);
  } else
    alert("The URI Format is invalid");  
}

function addTag(src, base, url)
{
  tgt = url;
  getTags(base);
  dlgsrc = src;
}

function closeMe()
{
  var d = document.getElementById("fhir-tag-form");
	document.getElementById("div-cnt").removeChild(d);
}

function tagType()
{
  document.getElementById("fhir-tag-input").value = "";
  document.getElementById("fhir-tag-text").value = "";
  if (document.getElementById("fhir-tag-tt").checked) 
    document.getElementById("fhir-tag-ul").innerHTML = tags_tag;  
  else if (document.getElementById("fhir-tag-tp").checked)
    document.getElementById("fhir-tag-ul").innerHTML = tags_profile;  
  else
    document.getElementById("fhir-tag-ul").innerHTML = tags_security;    
  new combo('fhir-tag-input','#9c9','#cfc'); 
}

function buildForm()
{
  var d = document.getElementById("fhir-tag-form");
  if (d == null)
  {
    d = document.createElement("div");
    d.id = "fhir-tag-form";
  }
  d.innerHTML = "<table><tr><td>Text:</td><td><input id=\"fhir-tag-text\" type=\"text\" name=\"label\" size=\"60\"></td><td><button onClick='commitTag()'>Commit</button></td></tr>"+
     "<tr><td>Type:</td><td><input id=\"fhir-tag-tt\" type=\"radio\" name=\"type\" value=\"t\" checked onClick='tagType()'> Tag&nbsp;<input id=\"fhir-tag-tp\" type=\"radio\" name=\"type\" value=\"p\" onClick='tagType()'> Profile&nbsp;<input id=\"fhir-tag-ts\" type=\"radio\" name=\"type\" value=\"s\" onClick='tagType()'> Security</td><td>&nbsp;</td></tr>"+
     "<tr><td>Uri:</td><td><div id=\"fhir-tag-div\" class=\"combo\"><input id=\"fhir-tag-input\" type=\"text\" name=\"uri\" size=\"60\"><ul id=\"fhir-tag-ul\" style=\"display: none;\"><li>item</li></ul></div></td><td><button onClick='closeMe()'>Close</button></td></tr></table>";
  d.className = "fhir-tag-form";
	document.getElementById("div-cnt").appendChild(d);
  src = document.getElementById(dlgsrc);  
  $('.fhir-tag-form').css('left', src.offsetLeft+src.offsetWidth);
  $('.fhir-tag-form').css('top', src.offsetTop+src.offsetHeight);
}

function process(data)
{
  buildForm();
  tags_tag = "";
  tags_profile = "";
  tags_security = "";
  tags = data.category;
  for (var i = 0; i < tags.length; i++) { 
    if (tags[i].scheme == "http://hl7.org/fhir/tag")
      tags_tag = tags_tag + "<li title=\""+tags[i].label+"\">"+tags[i].term+"</li>";
    else if (tags[i].scheme == "http://hl7.org/fhir/tag/profile")
      tags_profile = tags_profile + "<li title=\""+tags[i].label+"\">"+tags[i].term+"</li>";
    else if (tags[i].scheme == "http://hl7.org/fhir/tag/security")
      tags_security = tags_security + "<li title=\""+tags[i].label+"\">"+tags[i].term+"</li>";
  }
  document.getElementById("fhir-tag-ul").innerHTML = tags_tag;  
  new combo('fhir-tag-input','#9c9','#cfc'); 
}

function success(data) 
{
  window.location.reload();
}

function deleteTag(target, scheme, term)
{
  tgt = target;
  json = "{ \"resourceType\" : \"TagList\", \"category\" : [{ \"term\" : \""+term+"\", \"scheme\" : \""+scheme+"\" }] }";
  try
  {
    $.ajax({
      url: tgt,
      cache: false,
      type: "POST",
      data: json,
      dataType: "json",
      contentType: "application/json; charset=utf-8",
      success: function(data){
        success(data);
      },
      error: function(jqXHR, textStatus, errorThrown){
		if (errorThrown)
          alert("System Error: "+errorThrown);
		else
          alert("System Error: "+textStatus);
      }
    });
  }
  catch (err)
  {
    alert("System Error: "+err);
  }
}

function sendTag(json)
{
  try
  {
    $.ajax({
      url: tgt,
      cache: false,
      type: "post",
      data: json,
      dataType: "json",
      contentType: "application/json; charset=utf-8",
      success: function(data){
        success(data);
      },
      error: function(jqXHR, textStatus, errorThrown){
		if (errorThrown)
          alert("System Error: "+errorThrown);
		else
          alert("System Error: "+textStatus);
      }
    });
  }
  catch (err)
  {
    alert("System Error: "+err);
  }
}

function getTags(base)
{
  try
  {
    $.ajax({
      url: base+"_tags",
      cache: false,
      dataType: "json",
      success: function(data){
        process(data);
      },
      error: function(jqXHR, textStatus, errorThrown){
		if (errorThrown)
          alert("System Error: "+errorThrown);
		else
          alert("System Error: "+textStatus);
      }
    });
  }
  catch (err)
  {
    alert("System Error: "+err);
  }
}
