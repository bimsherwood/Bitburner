
export function numberWithMagnitude(number, fractionDigits){
    
    var base = 1000;
    var magnitudes = [
      ' ',
      'k',
      'm',
      'b',
      't',
      'q'
    ];
    
    var multiple = number;
    var multipleRounded = multiple.toFixed(fractionDigits);
    var exponent;
    for(exponent = 0; exponent < magnitudes.length - 1; exponent++){
      if(multipleRounded < base){
        break;
      } else {
        multiple = multiple / base;
        multipleRounded = multiple.toFixed(fractionDigits);
        continue;
      }
    }
    
    var mantissa = multipleRounded;
    var magnitude = magnitudes[exponent];
    
    return mantissa.toString() + magnitude;
    
}

export async function main(ns){

	ns.tprint("Testing formats:");
  
	ns.tprint(numberWithMagnitude(123.456, 1));
	ns.tprint(numberWithMagnitude(123.456, 2));
	ns.tprint(numberWithMagnitude(123.456, 4));

	ns.tprint(numberWithMagnitude(1123.456, 1));
	ns.tprint(numberWithMagnitude(1123.456, 2));
	ns.tprint(numberWithMagnitude(1123.456, 4));

	ns.tprint(numberWithMagnitude(1234567.456, 1));
	ns.tprint(numberWithMagnitude(1234567.456, 2));
	ns.tprint(numberWithMagnitude(1234567.456, 4));

	ns.tprint(numberWithMagnitude(1234567.456 * 1000, 1));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000, 2));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000, 4));

	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000, 1));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000, 2));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000, 4));

	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000 * 1000, 1));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000 * 1000, 2));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000 * 1000 * 1000, 4));

	ns.tprint(numberWithMagnitude(1234567.456 * 1000000 * 1000000, 1));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000000 * 1000000, 2));
	ns.tprint(numberWithMagnitude(1234567.456 * 1000000 * 1000000, 4));
  
	ns.tprint(numberWithMagnitude(1000 * 1000 * 1000 - 1, 2));

}