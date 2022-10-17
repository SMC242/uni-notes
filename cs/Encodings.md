# Encodings
## NRZ
- 1 is high, 0 is low
- Having the same value in succession causes issues

## NRZ Inverted
- 1 is a change in value, 0 is a constant signal
- Solves NRZ's issue of repeated 1s but not repeated 0s

## Manchester
- 1 is a high to low change, 0 is a low to high change
- Uses twice the bandwidth but has no issues with repeated digits