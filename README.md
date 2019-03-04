# Repositry Willis

## This Respository will be for Jack's Graph and Code

###Rating Curve for the River Aire

When planning flood mitigation, it is important to estimate the volume of water that the river will flood by, in order to see which flood alleviation schemes are most appropriate. We call this the flood excess volume (FEV). To calculate the FEV of a flood, first data must be collected and analyzed from the Environmental Agency. They analyze the river level (height) against a timestamp usually at 15 minute intervals across hundreds of gauge stations over the country. For each individual gauge station, it is possible to estimate the threshold level of the river *h<sub>T</sub>*, measured in m. This value can be estimated many different ways, from social media time stamped photos to online live resources such as (www.gaugemap.com). To analyze this data, a rating curve is created by the Environmental Agency to estimate the flow of water m<sup>3</sup>/s at the river’s given level and time. The frequent measurements taken at the gauge station can then be plotted onto a graph using the rating equation Q=Q(h), where Q is the flow of water m<sup>3</sup>/s. We can then apply *h<sub>T</sub>* into the rating curve to give the threshold flow *q<sub>T</sub>*=Q(*h<sub>T</sub>*). FEV can then be defined as: FEV=(Ve)=Integral((Q(t)-(*q<sub>T</sub>*))) over time *T<sub>f</sub>*, where *T<sub>f</sub>* is where Q(t)-*q<sub>T</sub>*>0. This gives a volume of water in which the river has flooded by. A way to estimate this value is to evaluate *h<sub>M</sub>*, the mean level of the river given the level is above *h<sub>T</sub>*. Again, this value can be applied into the rating equation to give a flow value *q<sub>M</sub>*. Thus,
![](FEV eqn.png).
This is the formula that will be used to estimate FEV from the quadrant plots that have been created for each river.


