<!DOCTYPE html>
<!-- saved from url=(0014)about:internet -->
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
<meta http-equiv="x-ua-compatible" content="IE=9" >

<title>Reproducible Research Peer Assessed, Assignment 1</title>

<style type="text/css">
body, td {
   font-family: sans-serif;
   background-color: white;
   font-size: 12px;
   margin: 8px;
}

tt, code, pre {
   font-family: 'DejaVu Sans Mono', 'Droid Sans Mono', 'Lucida Console', Consolas, Monaco, monospace;
}

h1 { 
   font-size:2.2em; 
}

h2 { 
   font-size:1.8em; 
}

h3 { 
   font-size:1.4em; 
}

h4 { 
   font-size:1.0em; 
}

h5 { 
   font-size:0.9em; 
}

h6 { 
   font-size:0.8em; 
}

a:visited {
   color: rgb(50%, 0%, 50%);
}

pre {	
   margin-top: 0;
   max-width: 95%;
   border: 1px solid #ccc;
   white-space: pre-wrap;
}

pre code {
   display: block; padding: 0.5em;
}

code.r, code.cpp {
   background-color: #F8F8F8;
}

table, td, th {
  border: none;
}

blockquote {
   color:#666666;
   margin:0;
   padding-left: 1em;
   border-left: 0.5em #EEE solid;
}

hr {
   height: 0px;
   border-bottom: none;
   border-top-width: thin;
   border-top-style: dotted;
   border-top-color: #999999;
}

@media print {
   * { 
      background: transparent !important; 
      color: black !important; 
      filter:none !important; 
      -ms-filter: none !important; 
   }

   body { 
      font-size:12pt; 
      max-width:100%; 
   }
       
   a, a:visited { 
      text-decoration: underline; 
   }

   hr { 
      visibility: hidden;
      page-break-before: always;
   }

   pre, blockquote { 
      padding-right: 1em; 
      page-break-inside: avoid; 
   }

   tr, img { 
      page-break-inside: avoid; 
   }

   img { 
      max-width: 100% !important; 
   }

   @page :left { 
      margin: 15mm 20mm 15mm 10mm; 
   }
     
   @page :right { 
      margin: 15mm 10mm 15mm 20mm; 
   }

   p, h2, h3 { 
      orphans: 3; widows: 3; 
   }

   h2, h3 { 
      page-break-after: avoid; 
   }
}

</style>

<!-- Styles for R syntax highlighter -->
<style type="text/css">
   pre .operator,
   pre .paren {
     color: rgb(104, 118, 135)
   }

   pre .literal {
     color: rgb(88, 72, 246)
   }

   pre .number {
     color: rgb(0, 0, 205);
   }

   pre .comment {
     color: rgb(76, 136, 107);
   }

   pre .keyword {
     color: rgb(0, 0, 255);
   }

   pre .identifier {
     color: rgb(0, 0, 0);
   }

   pre .string {
     color: rgb(3, 106, 7);
   }
</style>

<!-- R syntax highlighter -->
<script type="text/javascript">
var hljs=new function(){function m(p){return p.replace(/&/gm,"&amp;").replace(/</gm,"&lt;")}function f(r,q,p){return RegExp(q,"m"+(r.cI?"i":"")+(p?"g":""))}function b(r){for(var p=0;p<r.childNodes.length;p++){var q=r.childNodes[p];if(q.nodeName=="CODE"){return q}if(!(q.nodeType==3&&q.nodeValue.match(/\s+/))){break}}}function h(t,s){var p="";for(var r=0;r<t.childNodes.length;r++){if(t.childNodes[r].nodeType==3){var q=t.childNodes[r].nodeValue;if(s){q=q.replace(/\n/g,"")}p+=q}else{if(t.childNodes[r].nodeName=="BR"){p+="\n"}else{p+=h(t.childNodes[r])}}}if(/MSIE [678]/.test(navigator.userAgent)){p=p.replace(/\r/g,"\n")}return p}function a(s){var r=s.className.split(/\s+/);r=r.concat(s.parentNode.className.split(/\s+/));for(var q=0;q<r.length;q++){var p=r[q].replace(/^language-/,"");if(e[p]){return p}}}function c(q){var p=[];(function(s,t){for(var r=0;r<s.childNodes.length;r++){if(s.childNodes[r].nodeType==3){t+=s.childNodes[r].nodeValue.length}else{if(s.childNodes[r].nodeName=="BR"){t+=1}else{if(s.childNodes[r].nodeType==1){p.push({event:"start",offset:t,node:s.childNodes[r]});t=arguments.callee(s.childNodes[r],t);p.push({event:"stop",offset:t,node:s.childNodes[r]})}}}}return t})(q,0);return p}function k(y,w,x){var q=0;var z="";var s=[];function u(){if(y.length&&w.length){if(y[0].offset!=w[0].offset){return(y[0].offset<w[0].offset)?y:w}else{return w[0].event=="start"?y:w}}else{return y.length?y:w}}function t(D){var A="<"+D.nodeName.toLowerCase();for(var B=0;B<D.attributes.length;B++){var C=D.attributes[B];A+=" "+C.nodeName.toLowerCase();if(C.value!==undefined&&C.value!==false&&C.value!==null){A+='="'+m(C.value)+'"'}}return A+">"}while(y.length||w.length){var v=u().splice(0,1)[0];z+=m(x.substr(q,v.offset-q));q=v.offset;if(v.event=="start"){z+=t(v.node);s.push(v.node)}else{if(v.event=="stop"){var p,r=s.length;do{r--;p=s[r];z+=("</"+p.nodeName.toLowerCase()+">")}while(p!=v.node);s.splice(r,1);while(r<s.length){z+=t(s[r]);r++}}}}return z+m(x.substr(q))}function j(){function q(x,y,v){if(x.compiled){return}var u;var s=[];if(x.k){x.lR=f(y,x.l||hljs.IR,true);for(var w in x.k){if(!x.k.hasOwnProperty(w)){continue}if(x.k[w] instanceof Object){u=x.k[w]}else{u=x.k;w="keyword"}for(var r in u){if(!u.hasOwnProperty(r)){continue}x.k[r]=[w,u[r]];s.push(r)}}}if(!v){if(x.bWK){x.b="\\b("+s.join("|")+")\\s"}x.bR=f(y,x.b?x.b:"\\B|\\b");if(!x.e&&!x.eW){x.e="\\B|\\b"}if(x.e){x.eR=f(y,x.e)}}if(x.i){x.iR=f(y,x.i)}if(x.r===undefined){x.r=1}if(!x.c){x.c=[]}x.compiled=true;for(var t=0;t<x.c.length;t++){if(x.c[t]=="self"){x.c[t]=x}q(x.c[t],y,false)}if(x.starts){q(x.starts,y,false)}}for(var p in e){if(!e.hasOwnProperty(p)){continue}q(e[p].dM,e[p],true)}}function d(B,C){if(!j.called){j();j.called=true}function q(r,M){for(var L=0;L<M.c.length;L++){if((M.c[L].bR.exec(r)||[null])[0]==r){return M.c[L]}}}function v(L,r){if(D[L].e&&D[L].eR.test(r)){return 1}if(D[L].eW){var M=v(L-1,r);return M?M+1:0}return 0}function w(r,L){return L.i&&L.iR.test(r)}function K(N,O){var M=[];for(var L=0;L<N.c.length;L++){M.push(N.c[L].b)}var r=D.length-1;do{if(D[r].e){M.push(D[r].e)}r--}while(D[r+1].eW);if(N.i){M.push(N.i)}return f(O,M.join("|"),true)}function p(M,L){var N=D[D.length-1];if(!N.t){N.t=K(N,E)}N.t.lastIndex=L;var r=N.t.exec(M);return r?[M.substr(L,r.index-L),r[0],false]:[M.substr(L),"",true]}function z(N,r){var L=E.cI?r[0].toLowerCase():r[0];var M=N.k[L];if(M&&M instanceof Array){return M}return false}function F(L,P){L=m(L);if(!P.k){return L}var r="";var O=0;P.lR.lastIndex=0;var M=P.lR.exec(L);while(M){r+=L.substr(O,M.index-O);var N=z(P,M);if(N){x+=N[1];r+='<span class="'+N[0]+'">'+M[0]+"</span>"}else{r+=M[0]}O=P.lR.lastIndex;M=P.lR.exec(L)}return r+L.substr(O,L.length-O)}function J(L,M){if(M.sL&&e[M.sL]){var r=d(M.sL,L);x+=r.keyword_count;return r.value}else{return F(L,M)}}function I(M,r){var L=M.cN?'<span class="'+M.cN+'">':"";if(M.rB){y+=L;M.buffer=""}else{if(M.eB){y+=m(r)+L;M.buffer=""}else{y+=L;M.buffer=r}}D.push(M);A+=M.r}function G(N,M,Q){var R=D[D.length-1];if(Q){y+=J(R.buffer+N,R);return false}var P=q(M,R);if(P){y+=J(R.buffer+N,R);I(P,M);return P.rB}var L=v(D.length-1,M);if(L){var O=R.cN?"</span>":"";if(R.rE){y+=J(R.buffer+N,R)+O}else{if(R.eE){y+=J(R.buffer+N,R)+O+m(M)}else{y+=J(R.buffer+N+M,R)+O}}while(L>1){O=D[D.length-2].cN?"</span>":"";y+=O;L--;D.length--}var r=D[D.length-1];D.length--;D[D.length-1].buffer="";if(r.starts){I(r.starts,"")}return R.rE}if(w(M,R)){throw"Illegal"}}var E=e[B];var D=[E.dM];var A=0;var x=0;var y="";try{var s,u=0;E.dM.buffer="";do{s=p(C,u);var t=G(s[0],s[1],s[2]);u+=s[0].length;if(!t){u+=s[1].length}}while(!s[2]);if(D.length>1){throw"Illegal"}return{r:A,keyword_count:x,value:y}}catch(H){if(H=="Illegal"){return{r:0,keyword_count:0,value:m(C)}}else{throw H}}}function g(t){var p={keyword_count:0,r:0,value:m(t)};var r=p;for(var q in e){if(!e.hasOwnProperty(q)){continue}var s=d(q,t);s.language=q;if(s.keyword_count+s.r>r.keyword_count+r.r){r=s}if(s.keyword_count+s.r>p.keyword_count+p.r){r=p;p=s}}if(r.language){p.second_best=r}return p}function i(r,q,p){if(q){r=r.replace(/^((<[^>]+>|\t)+)/gm,function(t,w,v,u){return w.replace(/\t/g,q)})}if(p){r=r.replace(/\n/g,"<br>")}return r}function n(t,w,r){var x=h(t,r);var v=a(t);var y,s;if(v){y=d(v,x)}else{return}var q=c(t);if(q.length){s=document.createElement("pre");s.innerHTML=y.value;y.value=k(q,c(s),x)}y.value=i(y.value,w,r);var u=t.className;if(!u.match("(\\s|^)(language-)?"+v+"(\\s|$)")){u=u?(u+" "+v):v}if(/MSIE [678]/.test(navigator.userAgent)&&t.tagName=="CODE"&&t.parentNode.tagName=="PRE"){s=t.parentNode;var p=document.createElement("div");p.innerHTML="<pre><code>"+y.value+"</code></pre>";t=p.firstChild.firstChild;p.firstChild.cN=s.cN;s.parentNode.replaceChild(p.firstChild,s)}else{t.innerHTML=y.value}t.className=u;t.result={language:v,kw:y.keyword_count,re:y.r};if(y.second_best){t.second_best={language:y.second_best.language,kw:y.second_best.keyword_count,re:y.second_best.r}}}function o(){if(o.called){return}o.called=true;var r=document.getElementsByTagName("pre");for(var p=0;p<r.length;p++){var q=b(r[p]);if(q){n(q,hljs.tabReplace)}}}function l(){if(window.addEventListener){window.addEventListener("DOMContentLoaded",o,false);window.addEventListener("load",o,false)}else{if(window.attachEvent){window.attachEvent("onload",o)}else{window.onload=o}}}var e={};this.LANGUAGES=e;this.highlight=d;this.highlightAuto=g;this.fixMarkup=i;this.highlightBlock=n;this.initHighlighting=o;this.initHighlightingOnLoad=l;this.IR="[a-zA-Z][a-zA-Z0-9_]*";this.UIR="[a-zA-Z_][a-zA-Z0-9_]*";this.NR="\\b\\d+(\\.\\d+)?";this.CNR="\\b(0[xX][a-fA-F0-9]+|(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)";this.BNR="\\b(0b[01]+)";this.RSR="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|\\.|-|-=|/|/=|:|;|<|<<|<<=|<=|=|==|===|>|>=|>>|>>=|>>>|>>>=|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~";this.ER="(?![\\s\\S])";this.BE={b:"\\\\.",r:0};this.ASM={cN:"string",b:"'",e:"'",i:"\\n",c:[this.BE],r:0};this.QSM={cN:"string",b:'"',e:'"',i:"\\n",c:[this.BE],r:0};this.CLCM={cN:"comment",b:"//",e:"$"};this.CBLCLM={cN:"comment",b:"/\\*",e:"\\*/"};this.HCM={cN:"comment",b:"#",e:"$"};this.NM={cN:"number",b:this.NR,r:0};this.CNM={cN:"number",b:this.CNR,r:0};this.BNM={cN:"number",b:this.BNR,r:0};this.inherit=function(r,s){var p={};for(var q in r){p[q]=r[q]}if(s){for(var q in s){p[q]=s[q]}}return p}}();hljs.LANGUAGES.cpp=function(){var a={keyword:{"false":1,"int":1,"float":1,"while":1,"private":1,"char":1,"catch":1,"export":1,virtual:1,operator:2,sizeof:2,dynamic_cast:2,typedef:2,const_cast:2,"const":1,struct:1,"for":1,static_cast:2,union:1,namespace:1,unsigned:1,"long":1,"throw":1,"volatile":2,"static":1,"protected":1,bool:1,template:1,mutable:1,"if":1,"public":1,friend:2,"do":1,"return":1,"goto":1,auto:1,"void":2,"enum":1,"else":1,"break":1,"new":1,extern:1,using:1,"true":1,"class":1,asm:1,"case":1,typeid:1,"short":1,reinterpret_cast:2,"default":1,"double":1,register:1,explicit:1,signed:1,typename:1,"try":1,"this":1,"switch":1,"continue":1,wchar_t:1,inline:1,"delete":1,alignof:1,char16_t:1,char32_t:1,constexpr:1,decltype:1,noexcept:1,nullptr:1,static_assert:1,thread_local:1,restrict:1,_Bool:1,complex:1},built_in:{std:1,string:1,cin:1,cout:1,cerr:1,clog:1,stringstream:1,istringstream:1,ostringstream:1,auto_ptr:1,deque:1,list:1,queue:1,stack:1,vector:1,map:1,set:1,bitset:1,multiset:1,multimap:1,unordered_set:1,unordered_map:1,unordered_multiset:1,unordered_multimap:1,array:1,shared_ptr:1}};return{dM:{k:a,i:"</",c:[hljs.CLCM,hljs.CBLCLM,hljs.QSM,{cN:"string",b:"'\\\\?.",e:"'",i:"."},{cN:"number",b:"\\b(\\d+(\\.\\d*)?|\\.\\d+)(u|U|l|L|ul|UL|f|F)"},hljs.CNM,{cN:"preprocessor",b:"#",e:"$"},{cN:"stl_container",b:"\\b(deque|list|queue|stack|vector|map|set|bitset|multiset|multimap|unordered_map|unordered_set|unordered_multiset|unordered_multimap|array)\\s*<",e:">",k:a,r:10,c:["self"]}]}}}();hljs.LANGUAGES.r={dM:{c:[hljs.HCM,{cN:"number",b:"\\b0[xX][0-9a-fA-F]+[Li]?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+(?:[eE][+\\-]?\\d*)?L\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\b\\d+\\.(?!\\d)(?:i\\b)?",e:hljs.IMMEDIATE_RE,r:1},{cN:"number",b:"\\b\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"keyword",b:"(?:tryCatch|library|setGeneric|setGroupGeneric)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\.",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\.\\.\\d+(?![\\w.])",e:hljs.IMMEDIATE_RE,r:10},{cN:"keyword",b:"\\b(?:function)",e:hljs.IMMEDIATE_RE,r:2},{cN:"keyword",b:"(?:if|in|break|next|repeat|else|for|return|switch|while|try|stop|warning|require|attach|detach|source|setMethod|setClass)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"literal",b:"(?:NA|NA_integer_|NA_real_|NA_character_|NA_complex_)\\b",e:hljs.IMMEDIATE_RE,r:10},{cN:"literal",b:"(?:NULL|TRUE|FALSE|T|F|Inf|NaN)\\b",e:hljs.IMMEDIATE_RE,r:1},{cN:"identifier",b:"[a-zA-Z.][a-zA-Z0-9._]*\\b",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"<\\-(?!\\s*\\d)",e:hljs.IMMEDIATE_RE,r:2},{cN:"operator",b:"\\->|<\\-",e:hljs.IMMEDIATE_RE,r:1},{cN:"operator",b:"%%|~",e:hljs.IMMEDIATE_RE},{cN:"operator",b:">=|<=|==|!=|\\|\\||&&|=|\\+|\\-|\\*|/|\\^|>|<|!|&|\\||\\$|:",e:hljs.IMMEDIATE_RE,r:0},{cN:"operator",b:"%",e:"%",i:"\\n",r:1},{cN:"identifier",b:"`",e:"`",r:0},{cN:"string",b:'"',e:'"',c:[hljs.BE],r:0},{cN:"string",b:"'",e:"'",c:[hljs.BE],r:0},{cN:"paren",b:"[[({\\])}]",e:hljs.IMMEDIATE_RE,r:0}]}};
hljs.initHighlightingOnLoad();
</script>




</head>

<body>
<h1>Reproducible Research Peer Assessed, Assignment 1</h1>

<h2>Loading and preprocessing the data</h2>

<ol>
<li>Load the data</li>
</ol>

<pre><code class="r">activity&lt;-read.csv(&quot;activity/activity.csv&quot;)
head(activity)
</code></pre>

<pre><code>##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
</code></pre>

<ol>
<li>Process/transform the data (if necessary) into a format suitable for your analysis
remove NA</li>
</ol>

<pre><code class="r">activityComplete&lt;-activity[complete.cases(activity),]
head(activityComplete)
</code></pre>

<pre><code>##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
</code></pre>

<h2>What is mean total number of steps taken per day?</h2>

<ol>
<li>Calculate the total number of steps taken per day</li>
</ol>

<pre><code class="r">library(plyr); library(dplyr)
</code></pre>

<pre><code>## 
## Attaching package: &#39;dplyr&#39;
## 
## The following objects are masked from &#39;package:plyr&#39;:
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from &#39;package:stats&#39;:
## 
##     filter
## 
## The following objects are masked from &#39;package:base&#39;:
## 
##     intersect, setdiff, setequal, union
</code></pre>

<pre><code class="r">totalsteps&lt;-ddply(activityComplete,&quot;date&quot;,summarise,steps=sum(steps))
head(totalsteps)
</code></pre>

<pre><code>##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
</code></pre>

<ol>
<li>Make a histogram of the total number of steps taken each day</li>
</ol>

<pre><code class="r">hist(totalsteps$steps,breaks=20,xlab=&quot;Steps per Day&quot;,main=&quot;Frequency of Steps Taken&quot;,col=&quot;green&quot;)
</code></pre>

<p><img src="data:image/png;base64,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" alt="plot of chunk unnamed-chunk-4"/> </p>

<ol>
<li>Calculate and report the mean and median of the total number of steps taken per day</li>
</ol>

<pre><code class="r">avg&lt;-mean(totalsteps$steps)
med&lt;-median(totalsteps$steps)
avgmed&lt;-data.frame(c(&quot;Mean&quot;,&quot;Median&quot;),c(avg,med))
names(avgmed)&lt;-c(&quot;Measure&quot;,&quot;Number&quot;)
print(avgmed)
</code></pre>

<pre><code>##   Measure   Number
## 1    Mean 10766.19
## 2  Median 10765.00
</code></pre>

<h2>What is the average daily activity pattern?</h2>

<ol>
<li>Make a time series plot (i.e. type = &ldquo;l&rdquo;) of the 5-minute interval (x-axis) and the average 
number of steps taken, averaged across all days (y-axis)</li>
</ol>

<pre><code class="r">library(plyr); library(dplyr)
timeseries&lt;-ddply(activityComplete,&quot;interval&quot;,summarise,steps=mean(steps))
time1&lt;-strptime(timeseries$interval[1:12],&quot;%M&quot;)
time2&lt;-strptime(paste(&quot;0&quot;,as.character(timeseries$interval[13:120]),sep=&quot;&quot;),&quot;%H%M&quot;)
time3&lt;-strptime(timeseries$interval[121:288],&quot;%H%M&quot;)
timecol&lt;-c(time1,time2,time3)
timeseries$timeint&lt;-timecol
with(timeseries,plot(timeint,steps,type=&quot;l&quot;))
</code></pre>

<p><img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAAAaVBMVEUAAAAAACsAAFUAKysAK4AAVaorAAArACsrAFUrK4ArVaorgNRVAABVACtVAFVVqv+AKwCAKyuAK1WAVQCAgFWAqoCA1NSA1P+qVQCq1ICq///UgCvU/6rU////qlX/1ID//6r//9T///+v2bSNAAAACXBIWXMAAAsSAAALEgHS3X78AAAP/klEQVR4nO2dDVujSBZGy97WbUe7dWZ3x15dTeT//8iVEAgfBSnqAwrec56ZTgLcutc6AlUkRFOAJGbtAmAdEC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC9KiHgDOZNQfEAspAbxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLoig+28KWBPGiBIs/3J3e5Pv26hG7EtkWtiSh4j+fn06PH9/fZ8euRbaFLUmo+OOv187jnNi1yLawJWGPFyX4HH985By/RRjVi4J4USSnc9lWtiCSg7tsK1uQBNM5x09ur0e+lS2I4B5vsq1sSQSnc4gvERzVI74E8aLEmM7dvGzqWj3iS2IM7j6fHxC/NeJM537fIn5jRJrOvf3jB+I3RYTp3EP58Dacz+XavYgvYVQvCuJFQbwoiBcF8aIgXhTEi4J4URAvCuJFQbwokuKzLW1BEC8K4kVBvCh64k2RbWlLgnhRVMVnWtxyIF4UUfHcN4l4URAvCuJFERWfa3HLgXhRNMVnW9xyIF4UxIuCeFEQLwriRUG8KKric61uMRAvCuJFQbwoiBcF8aKoiTeIr0C8KIgXBfGiIF4UxIsiJt4g/gziRZETX1eVY3VLgnhREC8K4kVBvCiIF0VNfIH4CsSLgnhRtMSbAvFnEC8K4kVBvCiIFwXxoiBeFMSLgnhRgsUf7kzJt1eP2MVBfEOo+M/np9Pjx/f32bHLY0aeCxIq/vjrtfM4J3Z5EN/AHi9K8Dn++Litc7z1uSByo3rrc0EQL0qMwV15tB+e4nPsWcQ3RBB/GtAf/pgfuzyIb4gg/nD/3pnOmZrw6mKD+IZg8Y83f/9V7vH3TOc2Rfjg7vPZ3BYfTOc2BqN6URAvCuJFQbwo4aP689xtOLrLsGcR3xC8x38+P3jHLg7iG8IP9cefL96xS4P4Bs7xokiJN6Mv9EC8KIgXBfGiIF4UxIuCeFEQLwriRdEVn2F9S4J4URAvCuJFQbwoiBcF8aIgXhTEi4J4URAvCuJFQbwoiBcF8aIgXhRh8RkWuCCIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFGXxGVa4HIgXBfGiIF4UxIuCeFEQLwriRUG8KG7i376/vxnzFLXpFUD8BSfxx58vX/8dfgz/RnxA0yuA+Atu4n+9fu3ziN8Tjod6c/PywaF+TzC4E0VafIYlLoab+M9nY8xt3KZXAPEXnMRXfyP+bab5/Hp1WFF+NS6F66i++Tda0yuA+AuOo/rbYmyPP9yZkm/DX4r8OhXxF9z2+EdjRvR+PleTvI/v7zObXgHEXwgd1dfHf8t5IL9OtVSUX5ELESp+43t8hkUuhPN07vv/fr7YtjifBjZ6js+wyIVwnc4d7t8tO3VI0yuA+Auu07kv8Xuczi1WZHadMWOPf7Pu8dueziF+evXpkq3VO4M779TrkmA6Z2oCS4vPeuLz64zQS7Zb3+MXqnKu+PRVOYhvrttZj/Ubms5ZC0L8xOq543mnppcH8e4ZdvVBDMS7Z9jVx6sR757hfKjfx8erEe+eoTnHj328euIdW8S3smxT/NTHq6uPZXk0vTyId8/gUMDR/q7dHsX7/0R7FJ8kNgnriTdzQxEfk3Dxvj8S4lcF8e4ZEN/d1vNnMjN7wzvRnBQhq5PFJgHxnRQhq5PFJiFU/Fx9/pGIjwriOylCVieLTQLiOylCVieLTQLiOylCVieLTQLiOylCVieLTQLiOylCVieLTQLiOylCVieLTQLiOylCVieLTcJq4s28PIiPzJbEJ/8kPuJnxacWb+YGeIP4WfELik/cf4ifFY/48KaXB/HDTJ6rk8UmAfHDTJ6rk8UmYTPi2/+mAvGz4hEf3vTybEx82g6UFz+jTMRHaXp58hdvEJ8CxLdTBa1OFpuEPMW3VyA+CauKHw1FfHIiiPf7oeaJN4iPTJbizVB8vQTxkUB8vybv1clikxAofvKA7R2J+PTkK950XiA+MquLtwUjPj1riW/maJZgUwzFz67MBx3xY59cXVt8ewXiE7AV8aYfmAbEZyW+WyXiYzD2BfpLih9GIz45GYs3iE/IBsSbAvFxqabHiB8U5bs6WWxkqlnS2uJtFSA+KYgfKcp3dbLYyHSui1jWOTeC+PCmlySB+Bk/Xa0V8YuTrfjzOQjxiehdCu2uc26kszXik8RGJhfxg6h61Gmf8SE+FMSPFeW5OllsZKzjqnqdcyOdrRGfJDZyyqzFX6pDfOyUeYi3RNVt2sWn7ELEu5ZpWv/OCCsQvwDbFn/aAPExU2Yh3hY1FO9VmwcS4u2Xya8E2Te7iDe21dcSID4N42+5T3xDqFudpvOA+ESxvilzF98PQ3wc1hTvlsAmvmiJ9ynNi12Jn/isBeLnNY343laID246AYh3R0V88HRuTPxF3Vg7o+M1g/hIWDvfRBV/+QWIId50liLeP+O4+Kkot7Y7j97iWy+vi0/Yh/sWf97V44o3nVQh4g3i47Ci+IksOxV/uDMl3149YmNjEz/1qatmI6e2z4/GJr77jQaT7e9E/Ofz0+nx4/v77NjoDByb6u0ZZ/ETG46KN82/UuKPv147j3Nio2N6O70poom/KEL8ifz2+A2J3/So/viYzTm+6vzU4ruDu/qlh3hTbFp8mljvhP07UfoHgZGwTsDkRogfj6nxKigAT/Gdg+9Y70+L735b3Wjz3Zemv3BT4stBXXm0H57iV9rj29fQOzvldNz52di1v8H5IIF4W3jW4k8D+sMf82NjU4lvjKwh3kHefsQf7t/zmM71jDTirxXiJ76dbkr86Ge0Ni7+8ebvv8o9/n796VxffH3QdxY/MgMIEj++ZNviy5m8uS0+cpnOWcRfLaQr3m7qmvhqzUiwfcnWxSeJDUnYHOBzED9yAOkfHRAfQjLxZkp856TiKr7/1tFV8dE7E/FO4vuSAsWbwcjDPjBEvEu2rufWFaQ54q3fTOUsfhhsFV/v893trGW6/gyz2aV4M9yjpkPbkTbxZrD5VfGdw4+tUsTHoSd+TvKZ4usTcj35s4rvfwjXWmm3TcR7Zksofrh9R/xgP0f8Ypju3jbrHaL2CMomYFy8sYk3rRj7gM1HfOze3KX4mf10RbxtwHYeng3EX879E+Ltvw2I90u2ivjBob6oDzYT4kcOA4j3SxYs3swVbxnStcR3NrnSYIF4/2Q7Fl8fXqKCeH/xzcso4u0VI/5asl7PzRN/7t5mF+6tHmw/Ib770SzEpyZP8e5VIN47mf9XA5p6gI5419XJYj2S+XdNc3F/TfEjm9bSET+aLBPxxUX8nIquio/dnTsSHxTbuUxveqsH2ycQP7G8qgzxI8mWFG8mXrbEz6phfDniJ3MFpKvupu7du1rYXxVXxRf1SDFGByD+aq7VxNvevUP8QmQoPs7Pb5qzhrHWEtJwwOpksR65VhRvLyap+ODGEV/YxV+aW1X8WTrix3OFiC+64qu+Ns3zweaITxW7bK6qa3viC8SnaToqMcQXPfHn14iP3HRUchQfh5540ywObjdkdbLYZXPNFN9ftIr40AyIL6zi62P12Gehp5IjfjmCxRdj4m0tIz5Z7LK5BuLrD8znIr5oi+/+Bvi3G7I6WeyyuYZjcNP0dp7iw98IQHxhnXwNDwLuydOIb34PER8tF+Lnrk4Wu2wui/jzcq92E4i/NGsQHzeXVbxfs1HFd67ZnAb4iI+aKqr4mD+6aT2pZnaXQWdAHsRPNJGz+NHz09xmvVYni10+1VbEF4iPm8phvu7e1kLiQ/IgfqKJ7MUH/OCIT0BS8UXbPeKzIvqNblWrF/Ht6zn+rQWsThabbyoXUonvfAwD8cumciFROab/CvEa4vtJgq4QIj4By5RjEJ+Zd8QvRW7il6G5dOf14yN+syBeVPwJ7wu3iN82iBfFezaP+G2DeFEQLwriRZEWL+zd//od4jdOc5ff3Lig1cli80yUI0Pxbt2B+I0zEO94PQfxW6cj3v1CHuK3TvU+jalfuI7xEb8PavHOY3zE7wNz+RfxUpjuP27be69OFptnopxpOXfqjx2Ix3vJlHhbD21PvNOPpUdrVtfX7/LNXTNXJ4udbtb0F0D7vZqBeFsXbVV8fWTr/4VJXQbiWy+3Lv5su/mK8bl/42vX9P7AxuUa3trijXXpnBT11/6Y5vtAPN+Z2iVD8b0nvc2vtHY13+HOlHx7vRbbPgC1VrX3WGN51jqAXb7vyYz9HivT7b2OeEtXhYr/fH46PX58f78WOzxQF11/7d/Z5uJz/TdDOl8G0OzuYKPpVGNGD/eh4o+/XjuP52zGmMHJt1lWyTR1Wa3t26HtNi6RY38qCtoM/57O0MaVFq6lmLHHQ04En+OPj47neMiKTU3nIB6IFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoqQUDzmTTny6pjbRXv4FLpQs935AfKJkufcD4hMly70fEJ8oWe79gPhEyXLvB8QnSpZ7PyB+tWSQD4gXBfGiIF4UxIuCeFEQLwriRUG8KIgXJUT88dGU906fH2wLvNp7M/W9uYHtFcXhx2vE+uoGoxVYftnIU9wCnQkQX945/3ZbP9gWeLVX/H4aSzCXj9JPtPrqBqMVePz5Uhz++RKzQHcCxJffkfG1B5wfLs/OD77tff75Ur4Kb6/4ffOfr7Bo9dUNRivwo5T7+yligTMIEH+4fy9/ac8PtgVe7Z2+iOHJmmB+k1/dF62+usGYBUbtwFkEiC+/HOWrvvODbYFXe18Hv/NOFdheUXmKVl/dYMwCP58f4hboTH57/On56TSa6R4fscDj40MRt0Bnop3jbQu82js9P/Vr+CnvcDnHR2mvJz60wcNdq5U4BToTNKp/qMagD82gtLfAq73yQPf512t4e0XlKVp9dYPRCqy8xy3QmWjz+POwJ848/uYlRnudeXyU9pp5fJQC3043vDxFLdAZrtyJgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVRFV/ds2RdPni2S4TFB2+xaUTFHx/Nt/+WN2j+25iHj6//q0XV3Yv3/zLmqXq5X0TFnw/1n8+3xeHu9rR3/z7dxVAuvzvdz8Yev0vO4v+s7n8sb9L9+VLewnQ+948NAfYD4mvx5c3PNy+I3zlD8b9eL8sRv1sG4stzfHVmR/ye+XyuRvUX8V/H+upWyLP4ry32bF5VvDyIFwXxoiBeFMSLgnhREC8K4kVBvCiIFwXxoiBeFMSLgnhREC8K4kVBvCiIF+X/+qvzAma6h5wAAAAASUVORK5CYII=" alt="plot of chunk unnamed-chunk-6"/> </p>

<ol>
<li>Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?</li>
</ol>

<pre><code class="r">strftime(timeseries[which.max(timeseries$steps),3],format=&quot;%H:%M&quot;)
</code></pre>

<pre><code>## [1] &quot;08:35&quot;
</code></pre>

<h2>Imputing missing values</h2>

<ol>
<li>Calculate and report the total number of missing values in the dataset 
(i.e. the total number of rows with NAs)</li>
</ol>

<pre><code class="r">activityNA&lt;-activity[is.na(activity),]
length(activityNA[,1])
</code></pre>

<pre><code>## [1] 2304
</code></pre>

<ol>
<li>Devise a strategy for filling in all of the missing values in the dataset.
The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.</li>
</ol>

<pre><code class="r">        ## Step 1 add a column from timeseries joined on interval to get the average for that interval
timeseriesM&lt;-timeseries[,1:2]
names(timeseriesM)&lt;-c(&quot;interval&quot;,&quot;avgsteps&quot;)
activityM&lt;-merge(activity,timeseriesM,by=&quot;interval&quot;)
activityM&lt;-activityM[with(activityM,order(date,interval)),]
        ## Step 2 If the steps is NA replace it with the AVG for that interval
activityM$stepsC&lt;-activityM$steps
activityM$stepsC[is.na(activityM$stepsC)]&lt;-activityM$avgsteps[is.na(activityM$stepsC)]
        ## Now Column stepsC has complete values if the steps is NA
head(activityM)
</code></pre>

<pre><code>##     interval steps       date  avgsteps    stepsC
## 1          0    NA 2012-10-01 1.7169811 1.7169811
## 63         5    NA 2012-10-01 0.3396226 0.3396226
## 128       10    NA 2012-10-01 0.1320755 0.1320755
## 205       15    NA 2012-10-01 0.1509434 0.1509434
## 264       20    NA 2012-10-01 0.0754717 0.0754717
## 327       25    NA 2012-10-01 2.0943396 2.0943396
</code></pre>

<ol>
<li>Create a new dataset that is equal to the original dataset but with the missing data filled in.</li>
</ol>

<pre><code class="r">activityNew&lt;-activityM[,c(&quot;stepsC&quot;,&quot;date&quot;,&quot;interval&quot;)]
names(activityNew)&lt;-c(&quot;steps&quot;,&quot;date&quot;,&quot;interval&quot;)
head(activityNew)
</code></pre>

<pre><code>##         steps       date interval
## 1   1.7169811 2012-10-01        0
## 63  0.3396226 2012-10-01        5
## 128 0.1320755 2012-10-01       10
## 205 0.1509434 2012-10-01       15
## 264 0.0754717 2012-10-01       20
## 327 2.0943396 2012-10-01       25
</code></pre>

<ol>
<li>Make a histogram of the total number of steps taken each day and Calculate and report the mean 
and median total number of steps taken per day. Do these values differ from the estimates from the 
first part of the assignment? What is the impact of imputing missing data on the estimates of the 
total daily number of steps?</li>
</ol>

<pre><code class="r">totalstepsM&lt;-ddply(activityNew,&quot;date&quot;,summarise,steps=sum(steps))
hist(activityNew$steps,breaks=20,xlab=&quot;Steps per Day&quot;,main=&quot;Frequency of Steps Taken&quot;,col=&quot;blue&quot;)
</code></pre>

<p><img src="data:image/png;base64,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" alt="plot of chunk unnamed-chunk-11"/> </p>

<pre><code class="r">avgNew&lt;-mean(activityNew$steps)
medNew&lt;-median(activityNew$steps)
avgmedNew&lt;-data.frame(c(&quot;Mean&quot;,&quot;Median&quot;),c(avgNew,medNew))
names(avgmedNew)&lt;-c(&quot;Measure&quot;,&quot;Number&quot;)
print(avgmedNew)
</code></pre>

<pre><code>##   Measure  Number
## 1    Mean 37.3826
## 2  Median  0.0000
</code></pre>

<h2>Are there differences in activity patterns between weekdays and weekends?</h2>

<ol>
<li>Create a new factor variable in the dataset with two levels - &ldquo;weekday&rdquo; and &ldquo;weekend&rdquo; indicating 
whether a given date is a weekday or weekend day.</li>
</ol>

<pre><code class="r">library(chron)
activityNew$wkday&lt;-as.factor(weekdays(as.Date(activityNew$date)))
wkend&lt;-is.weekend(as.Date(activityNew$date))
activityNew$wkend&lt;-wkend
activityNew$wkend&lt;-as.factor(activityNew$wkend)
head(activityNew)
</code></pre>

<pre><code>##         steps       date interval  wkday wkend
## 1   1.7169811 2012-10-01        0 Monday FALSE
## 63  0.3396226 2012-10-01        5 Monday FALSE
## 128 0.1320755 2012-10-01       10 Monday FALSE
## 205 0.1509434 2012-10-01       15 Monday FALSE
## 264 0.0754717 2012-10-01       20 Monday FALSE
## 327 2.0943396 2012-10-01       25 Monday FALSE
</code></pre>

<ol>
<li>Make a panel plot containing a time series plot (i.e. type = &ldquo;l&rdquo;) of the 5-minute interval 
(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
(y-axis). See the README file in the GitHub repository to see an example of what this plot should 
look like using simulated data.</li>
</ol>

<pre><code class="r">library(plyr); library(dplyr)
timeseriesWkend&lt;-ddply(activityNew,c(&quot;interval&quot;,&quot;wkend&quot;),summarise,steps=mean(steps))
par(mfrow=c(2,1))
with(timeseriesWkend[timeseriesWkend$wkend==&quot;TRUE&quot;,],plot(interval,steps,type=&quot;l&quot;,col=&quot;blue&quot;))
with(timeseriesWkend[timeseriesWkend$wkend==&quot;FALSE&quot;,],plot(interval,steps,type=&quot;l&quot;,col=&quot;blue&quot;))
</code></pre>

<p><img src="data:image/png;base64,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" alt="plot of chunk unnamed-chunk-13"/> </p>

</body>

</html>

