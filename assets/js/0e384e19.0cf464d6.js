"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9671],{3905:function(e,t,n){n.d(t,{Zo:function(){return c},kt:function(){return d}});var r=n(7294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function a(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=r.createContext({}),u=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):a(a({},t),e)),n},c=function(e){var t=u(e.components);return r.createElement(s.Provider,{value:t},e.children)},p="mdxType",f={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,i=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),p=u(n),m=o,d=p["".concat(s,".").concat(m)]||p[m]||f[m]||i;return n?r.createElement(d,a(a({ref:t},c),{},{components:n})):r.createElement(d,a({ref:t},c))}));function d(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var i=n.length,a=new Array(i);a[0]=m;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[p]="string"==typeof e?e:o,a[1]=l;for(var u=2;u<i;u++)a[u]=n[u];return r.createElement.apply(null,a)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},9881:function(e,t,n){n.r(t),n.d(t,{assets:function(){return c},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return l},metadata:function(){return u},toc:function(){return p}});var r=n(3117),o=n(102),i=(n(7294),n(3905)),a=["components"],l={sidebar_position:1},s="Welcome to GeoLift",u={unversionedId:"intro",id:"intro",title:"Welcome to GeoLift",description:"---",source:"@site/docs/intro.md",sourceDirName:".",slug:"/intro",permalink:"/GeoLift/docs/intro",draft:!1,editUrl:"https://github.com/facebookincubator/GeoLift/docs/intro.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",next:{title:"Getting Started with GeoLift",permalink:"/GeoLift/docs/GettingStarted/InstallingR"}},c={},p=[{value:"What is GeoLift?",id:"what-is-geolift",level:2},{value:"Why GeoLift?",id:"why-geolift",level:2},{value:"Getting Started",id:"getting-started",level:2}],f={toc:p};function m(e){var t=e.components,n=(0,o.Z)(e,a);return(0,i.kt)("wrapper",(0,r.Z)({},f,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"welcome-to-geolift"},"Welcome to GeoLift"),(0,i.kt)("hr",null),(0,i.kt)("h2",{id:"what-is-geolift"},"What is GeoLift?"),(0,i.kt)("p",null,"GeoLift is Meta's open-source solution uses to measure Lift at a geographic level. This methodology leverages the latest developments in Synthetic Control Methods (SCM) to generate geographic quasi-experiments that measure the true incremental value of your marketing campaigns."),(0,i.kt)("hr",null),(0,i.kt)("h2",{id:"why-geolift"},"Why GeoLift?"),(0,i.kt)("p",null,"Five reasons why you should use GeoLift:"),(0,i.kt)("ol",null,(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("strong",{parentName:"li"},"It\u2019s grounded in incrementality"),", arguably the best indicator of return on investment."),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("strong",{parentName:"li"},"It\u2019s an end-to-end solution"),", spanning data ingestion, power calculations, test design, and more.\u200b"),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("strong",{parentName:"li"},"It\u2019s flexible"),". It\u2019s useful across many use cases, including omni-channel and cross-channel measurement."),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("strong",{parentName:"li"},"It\u2019s resilient"),". It relies on data at a designated market area level, not personal information impacted by cookie loss.\u200b"),(0,i.kt)("li",{parentName:"ol"},(0,i.kt)("strong",{parentName:"li"},"It\u2019s open source"),". You can use it freely, build on top of it, collaborate with us, and replicate results.  \u200b")),(0,i.kt)("hr",null),(0,i.kt)("h2",{id:"getting-started"},"Getting Started"),(0,i.kt)("p",null,"Get started by ",(0,i.kt)("strong",{parentName:"p"},(0,i.kt)("a",{parentName:"strong",href:"https://github.com/facebookincubator/GeoLift"},"installing the GeoLift R package")),"."),(0,i.kt)("p",null,"Or try our ",(0,i.kt)("strong",{parentName:"p"},(0,i.kt)("a",{parentName:"strong",href:"./GettingStarted/Walkthrough"},"GeoLift walkthrough")),"."))}m.isMDXComponent=!0}}]);