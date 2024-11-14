var W1 = [[ 0.11928815,  0.96177489, -0.88534677, -0.61436701],
 [ 0.22295256, -0.96505272, -0.07509675,  0.30369475],
 [ 0.06579537,  0.62901199,  0.48732617, -0.13961439],
 [-1.04665172, -0.52093518, -0.59125906,  0.20394436],
 [-0.51478803, -0.23138931, -0.46868369,  1.17855048]];
var b1 = [[ 0.19601935],
 [-0.19342837],
 [ 0.31806174],
 [-0.07144754],
 [-0.13470015]];
var W2 = [[-0.34254122,  0.38377631, -0.52172703, -0.07543647,  0.46100044],
 [-1.07109475, -0.75702542, -0.04695078,  0.7300579,   0.47207794],
 [ 0.30539933, -0.19134524, -0.43902811, -0.21625584, -0.26142278],
 [ 0.56572676, -0.15814479, -0.22530283, -0.51300699,  0.71702611],
 [-0.57403547,  0.53049827,  0.30684277,  0.72402579, -0.96297407],
 [-0.06621911, -0.11180497, -0.05057788, -0.26148099, -0.02918742],
 [ 0.18864946,  0.17635874, -0.60312462,  0.70077378,  0.45837009]];
var b2 = [[-0.44360825],
 [-0.07266946],
 [-0.22566286],
 [ 0.22009973],
 [-0.47234556],
 [ 0      ],
 [-0.60597926]];
var W3 = [[-0.71402669,  1.50594521, -0.65308839,  0.96633965,  1.41423416, -0.52293307,
  -1.02770114]];
var b3 = [[-0.45336807]];

var Utrain = [[  13.91402721],
 [  61.49773788],
 [ 129.47964478],
 [  10.85972881]];
var STDtrain = [[  5.6924839 ],
 [ 11.76528549],
 [ 64.00527191],
 [  4.62173271]];


function multiply(a, b) {
  var aNumRows = a.length, aNumCols = a[0].length,
      bNumRows = b.length, bNumCols = b[0].length,
      m = new Array(aNumRows);  // initialize array of rows
  for (var r = 0; r < aNumRows; ++r) {
    m[r] = new Array(bNumCols); // initialize the current row
    for (var c = 0; c < bNumCols; ++c) {
      m[r][c] = 0;             // initialize the current cell
      for (var i = 0; i < aNumCols; ++i) {
        m[r][c] += a[r][i] * b[i][c];
      }
    }
  }
  return m;
}

var relu = function (x) {
	 return Math.max(0, x);
};

var sigmoid = function (x) {
	return 1 / (1 + Math.pow(Math.E, -x));
	
};

var subtract = function (x,y) {
	return x-y;
	
};

var divide = function (x,y) {
	return x/y;
	
};

var add = function (x,y) {
	return x+y;
	
};

function matrixOp(X, func) {
	var xNumRows = X.length, xNumCols = X[0].length;
	
	for (var i = 0; i<xNumRows; ++i) {
		for (var j = 0; j<xNumCols; ++j) {
		   X[i][j] = func(X[i][j]);
		}
	}
	
}

function matrixOp2(X, Y, func) {
	var xNumRows = X.length, xNumCols = X[0].length;
	
	for (var i = 0; i<xNumRows; ++i) {
		for (var j = 0; j<xNumCols; ++j) {
		   X[i][j] = func(X[i][j], Y[i][j]);
		}
	}	
}

function matrixOp2prop(X, Y, func) {
	var xNumRows = X.length, xNumCols = X[0].length;
	
	for (var i = 0; i<xNumRows; ++i) {
		for (var j = 0; j<xNumCols; ++j) {
		   X[i][j] = func(X[i][j], Y[i][0]);
		}
	}	
}
function display(m) {
	
  var retval = '';
  for (var r = 0; r < m.length; ++r) {
    retval+=('&nbsp;&nbsp;'+m[r].join(' ')+'\n');
  }
	
  return retval;
}



module.exports = function forward_propagation(X) {
	matrixOp2(X,Utrain, subtract);

    matrixOp2(X,STDtrain, divide);
    
    
	X = multiply(W1,X);
	matrixOp2prop(X,b1,add);
	matrixOp(X,relu);
	X = multiply(W2,X);
	matrixOp2prop(X,b2,add);
	matrixOp(X,relu);
	X = multiply(W3,X);
	matrixOp2prop(X,b3,add);
	matrixOp(X,sigmoid);
	
	return Math.round(X[0][0]);
	
}