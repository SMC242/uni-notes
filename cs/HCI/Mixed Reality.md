---
aliases: XR
---
# Overview
Mixed reality is a mix of real and virtual experiences (all senses, not just visual)

![Milgram's reality-virtuality continuum](https://www.researchgate.net/publication/321405854/figure/fig1/AS:567028507475975@1512201539715/Milgram-and-Kishinos-Mixed-Reality-on-the-Reality-Virtuality-Continuum-Milgram-and.png)

# Mixed reality tracking
![[Mixed Reality Tracking#Overview]]

# Perspective-dependent rendering
- Virtual content appears in the correct place
	- E.G a mug appears on the table in the right orientation
- Requires lots of [[Sensors]]
	- Rotation
	- Position
	- Controller tracking (sometimes)
	- Hand tracking (sometimes)

# Augmented reality
![[Augmented Reality#Overview]]

# Virtual reality
![[Virtual Reality#Overview]]

# Spatial relationships
There are two frames of reference when rendering the virtual content

## Egocentric
When the content is placed relative to the viewer

Pros:
- Portable: content moves with the user
- Consistent: content is always in the same place relative to the user's view
- Reachable: the content is always within reach

Cons:
- Integration: not part of the environment
- Obtrusive: content can obscure reality in annoying ways
- Single-user: can't be shared due to no shared frame of reference
- Instability: content can be shaky

## Exocentric
When the content is placed relative to the world

Pros:
- Integration: the content becomes part of the world
	- More [[Virtual Reality#Immersion|immersive]]
- Optimisation: the content can be moved according to the environment
- Implicit haptics (the surfaces exist already)
- Collaboration: multiple users can interact with the same content

Cons:
- Needs [[Mixed Reality Tracking#6 DoF|6 DoF tracking]]
- Access: content has to be in a fixed location
- Limited discoverability and reachability

## Choosing
It's rarely straight-forward, but here are some guidelines:

Choose egocentric positioning when:
- The content is coupled with a context local to the user (E.G a speed meter while cycling)

Choose exocentric when:
- The content is coupled with an external context (E.G displaying the menu of a restaurant)