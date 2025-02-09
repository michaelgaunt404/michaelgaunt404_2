library(leaflet)
library(dplyr)
#Creates data
data("breweries91",package="leaflet")
#set.seed(1);
breweries91$goodbear<-sample(as.factor(c("terrific","marvelous","culparterretaping")),nrow(breweries91),replace=T)
#Colors
joliepalette<-c("red","green","blue")[1:nlevels(breweries91$goodbear)]
getColor <- function(breweries91) {joliepalette[breweries91$goodbear]}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(breweries91)
)

#Generate the javascript

jsscript3 <-
  paste0(
    "function(cluster) {
const groups= [",paste("'",levels(breweries91$goodbear),"'",sep="",collapse=","),"];
const colors= {
groups: [",paste("'", joliepalette,"'",sep="",collapse=","),"],
center:'#ddd',
text:'black'
};
const markers = cluster.getAllChildMarkers();

const proportions = groups.map(group => markers.filter(marker => marker.options.group === group).length / markers.length);
function sum(arr, first= 0, last) {
return arr.slice(first, last).reduce((total, curr) => total+curr, 0);
}
const cumulativeProportions= proportions.map((val, i, arr) => sum(arr, 0, i+1));
cumulativeProportions.unshift(0);

const width = 2*Math.sqrt(markers.length);
const radius= 15+width/2;

const arcs= cumulativeProportions.map((prop, i) => { return {
x   :  radius*Math.sin(2*Math.PI*prop),
y   : -radius*Math.cos(2*Math.PI*prop),
long: proportions[i-1] >.5 ? 1 : 0
}});
const paths= proportions.map((prop, i) => {
if (prop === 0) return '';
else if (prop === 1) return `<circle cx='0' cy='0' r='${radius}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`;
else return `<path d='M ${arcs[i].x} ${arcs[i].y} A ${radius} ${radius} 0 ${arcs[i+1].long} 1 ${arcs[i+1].x} ${arcs[i+1].y}' fill='none' stroke='${colors.groups[i]}' stroke-width='${width}' stroke-alignment='center' stroke-linecap='butt' />`
});

return new L.DivIcon({
html: `
<svg width='60' height='60' viewBox='-30 -30 60 60' style='width: 60px; height: 60px; position: relative; top: -24px; left: -24px;' >
<circle cx='0' cy='0' r='15' stroke='none' fill='${colors.center}' />
<text x='0' y='0' dominant-baseline='central' text-anchor='middle' fill='${colors.text}' font-size='15'>${markers.length}</text>
${paths.join('')}
</svg>
`,
className: 'marker-cluster'
});
}")

# Generates the map.
leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data=breweries91,
                    group=~goodbear,
                    icon = icons,
                    clusterOptions = markerClusterOptions(
                      iconCreateFunction =
                        JS(jsscript3)))
