//
// F# image processing functions.
//
// More details?
//
// Daniyal Khokhar
// UIC
// 3/22/2023

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //
  //skip_all, trim
  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
   // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //

  let pixelWork row =
    let r, g, b = row //deconstruct first tuple
    let rgb = (int)((float r)*0.299 + (float g)*0.587 + (float b)*0.114)
    let pixel = (rgb, rgb, rgb)
    pixel
    
  let rec grayOut newRow row = //This function will update an entire row
   //let pixel = pixelWork((List.head row)) //pixel is a tuple, (list.head row is first tuple)
    match row with
    | [] -> List.rev newRow
    | head::tail -> grayOut (pixelWork(List.head row)::newRow) tail
    
  
  let rec GrayHelp newImage image =
    let newRow = []
    match image with
    | [] -> newImage
    | head::tail -> GrayHelp ((grayOut newRow head)::newImage) tail //Pass a single row (list of tuples)
  
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    
    let newImage = []
    let finalImage = List.rev (GrayHelp newImage image)
    
    finalImage
    // let row = List.head image //get a list of tuples
    // let r, g, b = List.head row //deconstruct first tuple?
    // let rgb = ((float r)*0.299 + (float g)*0.587 + (float b)*0.114)
    // let pixel = (rgb, rgb, rgb)
    
    // for now, just return the image back, i.e. do nothing:
    //newImage


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rgbLimit e limit =
    if e <= limit then 
      0
    else
      255

  let pixelThresh row limit =
    let x, y, z = row //deconstruct first tuple
    let r = rgbLimit x limit
    let g = rgbLimit y limit
    let b = rgbLimit z limit
    
    let pixel = (r, g, b)
    pixel
    // let pixel = (100, 100, 100)
    // pixel
    
  let rec threshOut newRow row limit = //This function will update an entire row
   //let pixel = pixelWork((List.head row)) //pixel is a tuple, (list.head row is first tuple)
    match row with
    | [] -> List.rev newRow
    //| head::tail -> threshOut ((pixelThresh((List.head row) limit))::newRow) tail limit
    | head::tail -> threshOut ((pixelThresh (List.head row) limit)::newRow) tail limit
  
  let rec ThrHelp newImage image limit =
    let newRow = []
    match image with
    | [] -> newImage
    | head::tail -> ThrHelp ((threshOut newRow head limit)::newImage) tail limit//Pass a single row (list of tuples)
  
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    let newImage = []
    let finalImage = List.rev (ThrHelp newImage image threshold)
    
    finalImage


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  
  let rec FlipHorizHelp newImage image =
    match image with
    | []-> newImage
    | head::tail -> FlipHorizHelp ((List.rev head)::newImage) tail //concatentate new newImage list with next reversed row from image

    //match image with
    // | []-> newImage
    // | head::tail when head = List.head newImage -> FlipHorizHelp newImage tail
    // | head::tail when head <> List.head newImage -> FlipHorizHelp ((List.rev head)::newImage)   tail //concatentate new newImage list with next reversed row from image
    // | _ -> newImage

  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:

    
    //let row = List.head image
    //let rowRev = List.rev row
    //let newImage = rowRev::[] //add first element only
    let newImage = []
    let finalImage = List.rev (FlipHorizHelp newImage image)
    finalImage
    // match image with 
    //  [] -> []
    //  head::tail -> 
    
    
    //newImage


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let edgePixel x1 y1 z1 x2 y2 z2 =
    sqrt (float ( pown (x1-x2) 2 + pown (y1-y2) 2 + pown (z1-z2) 2))
    
  let pixelE curPixel rPixel bPixel limit =
    let cx, cy, cz = curPixel //deconstruct first tuple
    let rx, ry, rz = rPixel
    let bx, by, bz = bPixel
    let rFinal = edgePixel cx cy cz rx ry rz 
    let bFinal = edgePixel cx cy cz bx by bz 
    let black = (0, 0, 0)
    let white = (255, 255, 255)
    
    if (rFinal > float limit) || (bFinal > float limit) then 
      black
    else
      white
      
  let rec EOut newRow rowA rowB limit = //This function will update an entire row
    match rowA with
    | [] -> List.rev newRow
    | headA::tailA when tailA = [] -> List.rev newRow //there is no pixel to the right
    | headA::tailA -> 
      match rowB with 
      | [] -> List.rev newRow // there is no bottom pixel
      | headB::tailB -> EOut ((pixelE headA (List.head tailA) headB limit)::newRow) tailA tailB limit
    //Eout (rowA current pixel, rowA pixel to right, rowB pixel, our limit)
  
  let rec EHelp newImage image limit = //limit = threshold
    let newRow = [] //create new row of pixels
    match image with
    | [] -> newImage
    | row1::row2::tail -> EHelp ((EOut newRow row1 row2 limit)::newImage) (row2::tail) limit 
    | _ -> newImage
  
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    
    let newImage = []
    let finalImage = List.rev (EHelp newImage image threshold)
    
    finalImage
    

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
    
  // let rec RotateOut newRow row = //This function will update an entire row
  //   match row with
  //   | [] -> List.rev newRow
  //   //| head::tail -> grayOut (pixelWork(List.head row)::newRow) tail
    
  // let rec RotateHelp newImage image =
  //   let newRow = []
  //   match image with
  //   | [] -> newImage
  //   | head::tail -> RotateHelp ((RotateHelp newRow head)::newImage) tail //Pass a single row (list of tuples)
  
  
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    // let newImage = []
    // let finalImage = List.rev (RotateHelp newImage image)
    // finalImage
    List.transpose (List.rev image)
