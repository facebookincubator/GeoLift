"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[4571],{3905:function(e,t,a){a.d(t,{Zo:function(){return u},kt:function(){return d}});var r=a(7294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},o=Object.keys(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)a=o[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var l=r.createContext({}),c=function(e){var t=r.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},u=function(e){var t=c(e.components);return r.createElement(l.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},p=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,o=e.originalType,l=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),p=c(a),d=n,f=p["".concat(l,".").concat(d)]||p[d]||m[d]||o;return a?r.createElement(f,i(i({ref:t},u),{},{components:a})):r.createElement(f,i({ref:t},u))}));function d(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=a.length,i=new Array(o);i[0]=p;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s.mdxType="string"==typeof e?e:n,i[1]=s;for(var c=2;c<o;c++)i[c]=a[c];return r.createElement.apply(null,i)}return r.createElement.apply(null,a)}p.displayName="MDXCreateElement"},4002:function(e,t,a){a.r(t),a.d(t,{frontMatter:function(){return s},contentTitle:function(){return l},metadata:function(){return c},toc:function(){return u},default:function(){return p}});var r=a(3117),n=a(102),o=(a(7294),a(3905)),i=["components"],s={sidebar_position:1},l="GeoLift Best Practices",c={unversionedId:"Best Practices/BestPractices",id:"Best Practices/BestPractices",title:"GeoLift Best Practices",description:"---",source:"@site/docs/Best Practices/BestPractices.md",sourceDirName:"Best Practices",slug:"/Best Practices/BestPractices",permalink:"/GeoLift/docs/Best Practices/BestPractices",editUrl:"https://github.com/facebookincubator/GeoLift/docs/Best Practices/BestPractices.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Comparison to other Methodologies",permalink:"/GeoLift/docs/Methodology/ComparisonOthers"},next:{title:"GeoLift FAQs",permalink:"/GeoLift/docs/Best Practices/FAQs"}},u=[{value:"Data",id:"data",children:[],level:2},{value:"Test and Control Markets",id:"test-and-control-markets",children:[],level:2},{value:"Local Marketing Efforts",id:"local-marketing-efforts",children:[],level:2},{value:"National Marketing Efforts",id:"national-marketing-efforts",children:[],level:2}],m={toc:u};function p(e){var t=e.components,a=(0,n.Z)(e,i);return(0,o.kt)("wrapper",(0,r.Z)({},m,a,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"geolift-best-practices"},"GeoLift Best Practices"),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"data"},"Data"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Daily granularity in the data is strongly recommended over weekly.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"It is recommended to use the highest level of geographical granularity at which we can target Facebook campaigns in the study's region (Zip Codes, Cities, etc.).")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"We recommend having at least 4 - 5x the test duration of pre-campaign historical data of stable data (must not contain structural changes or any other impactful deviation from their data-generating process.).")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"At minimum we recommend having 25 pre-treatment periods of 20 or more geo-units.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Under normal circumstances we advise having historical information to be over the last 52 weeks, this may take into consideration any seasonal variations across brand product sales as well as account for other factors that may not be taken into consideration over a shorter duration.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"The test duration should cover at least one purchase cycle for the product.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Study should be run for a minimum of 15 days, if daily, and a minimum of 4-6 weeks if weekly to ensure enough data points are available to assess market impact.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Panel data of covariates is recommended to improve the model but not necessary.")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},"Date, location, and units (or any other KPI) must be had for each time/location combination (no missing values for any unit or timestamp). Additional covariates can be used but follow the same guideline."),(0,o.kt)("hr",{parentName:"li"}))),(0,o.kt)("h2",{id:"test-and-control-markets"},"Test and Control Markets"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Match test and control markets on variables such as sales plus other relevant variables that are specific to the brand category (e.g. product distribution, seasonal variations across geos)."),(0,o.kt)("li",{parentName:"ul"},"Test and control markets should be matched on the exact same outcome of interest, doing this may eliminate any bias in the results across markets.")),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"local-marketing-efforts"},"Local Marketing Efforts"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Account for any local media efforts such as local TV, any regional offline marketing efforts across the selection markets as these may contribute to some unbalancing factors when comparing results across control and test markets."),(0,o.kt)("li",{parentName:"ul"},"Keep all local marketing efforts to be constant across all markets that are taken into consideration across test and control.")),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"national-marketing-efforts"},"National Marketing Efforts"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Take into consideration any significant variations across national media such as TV, Print etc. during pre-test or test period will make it hard to really isolate the impact of Facebook on the outcome of interest."),(0,o.kt)("li",{parentName:"ul"},"For sales to be truly attributed to Facebook variations, all other media should be held constant across the markets and if there are significant variations, make sure to address these before the test.")))}p.isMDXComponent=!0}}]);