# Overview
When approaching new systems, users use the following tools to understand them:
- [[#Affordance]]
- [[#Knowledge]]
- [[#Constraints]]
- [[#Convention]]

# Signifiers
- Used to signal that something is there
- E.G a sign, an image, a theme song
- Two types:
	- Deliberate: things that have been created on purpose (signs)
	- Emergent: things that were coincidentally created (well-trodden paths, queues)

# Affordance
- What the individual thinks the system is for based on its appearance
	- E.G a chair might be for sitting
	- Not all people will see the same thing. A knife can mean multiple things - a weapon, a kitchen tool, a survival tool
- Forms a relationship between the object and an individual
	- The affordance of an object is not a property of the object itself
	- It's an interpretation of the object

> [!NOTE] Example parameters
> - Materials
> - Textures
> - Size
> - Height
> - The individual's capabilities (someone in a wheelchair may not see stairs as an entrance)
> - The individual's past experience

## Anti-affordances
- Objects that exist to prevent interaction
- E.G speedbumps, anti-homeless benches, anti-pigeon spikes on ledges

# Knowledge
- Things that an individual knows
- You don't need to know all of the ins and outs of a task to complete it
	- Just the broad strokes
- Two types:
	- Knowledge of the world
		- Perceived [[#affordance]]s
		- Controls
		- The actions available
	- Personal knowledge (stored "in our heads")
		- Memories
		- Past experience

## Memory
- Short term memory (STM) can only hold so much
	- $7 \pm 2$ items
	- Prone to distraction
	- Duration: 30 seconds
- Long term memory (LTM)
	- Arbitrary things
	- Meaningful things
	- Retaining and retrieving memories is easier when the information makes sense

# Constraints
- Constraints limit the possible uses of a system
- They are useful for preventing errors and guiding users towards intended behaviours

3 types:
> [!NOTE] Physical constraints
> - What is physically possible
> - An SD card will not fit in a USB port

> [!NOTE] Cultural constraints
> - Behaviours learned from others
> - Hold knife in right hand, fork in left

> [!NOTE] Semantic constraints
> - Intrinsic meaning
> - An egg is not bread, so it does not belong in the bread bin

> [!NOTE] Logical constraints
> - Reasoning
> - Cars can't drive on both sides of the road because they would collide

# Convention
- Try to be consistent with conventions
	- This makes it easier for users to go between systems
- If it's not possible to encode conventions into the design, use a [[#Constraints|cultural constraint]]
- Standardisation can enforce a convention where there is no natural choice
	- E.G little-endian vs big-endian binary encoding: CPU architectures pick one and stick with it

## Progress
- A design may be better, but people don't like to learn new things
- Only change conventions when the change provides a large and obvious benefit
- Mixed systems are confusing
- Standards inhibit future development
	- They make it harder to change things
	- Use them as a last resort