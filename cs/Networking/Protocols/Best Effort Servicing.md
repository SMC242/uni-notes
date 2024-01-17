---
tags: []
---
# Definition
- The network does its best to delivery the [[Packets]], but won't guarantee it
- [[Packets]] may be lost, delayed, disordered, duplicated, or corrupted

# Advantage
- Higher [[Protocol Layering|layers]] can implement reliability *if* they want to
- Not all protocols need reliability

## Examples
- [[TCP]] implements reliability, but it's slower because of it
- [[UDP]] doesn't need reliability and instead focuses on speed