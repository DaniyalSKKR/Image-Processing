# Image Processing Application

This application performs various image processing operations on PPM image files. The operations include converting to grayscale, thresholding, flipping horizontally, edge detection, and rotating 90 degrees to the right.

## Table of Contents

- [Getting Started](#getting-Started)
- [Usage](#usage)
- [Operations](#operations)
- [Dependencies](#dependencies)

## Getting Started

### Prerequisites

To run this application, you need to have the following installed on your machine:

- .NET SDK
- F# support in .NET

## Usage

1. Build the project:
    ```sh
    dotnet build
    ```

2. Run the application:
    ```sh
    dotnet run
    ```

3. Follow the prompts in the console to input the image filename and select the desired operation.

## Operations

### Grayscale

Converts the image to grayscale using a weighted average calculation for the RGB values.

### Threshold

Performs a threshold operation, setting RGB values above a specified threshold to the color depth and values below to 0.

### Flip Horizontal

Flips the image horizontally so that the left side becomes the right side and vice versa.

### Edge Detect

Detects edges in the image by comparing each pixel to its right and bottom neighbors. If the color difference exceeds a specified threshold, the pixel is marked as an edge.

### Rotate Right 90

Rotates the image 90 degrees to the right.

## Dependencies

- .NET SDK
- F# Core Library

Ensure you have these installed before running the application.
