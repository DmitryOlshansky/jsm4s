package jsm4s.ds;

public class UIntSetOps {

    final static class Result {
        int ai, bi, ci;
        Result(int ai, int bi, int ci){
            this.ai = ai;
            this.bi = bi;
            this.ci = ci;
        }
    }

    private static Result blockwise2(long[] a, int aSize, long[] b, int bSize, long[] c)
    {
        int ai = 0, bi = 0,  ci = 0;
        long a0 = a[ai], a1 = a[ai+1];
        long b0 = b[bi], b1 = b[bi+1];
        for(;;){

            if(a0 == b0){
                c[ci++] = a0;
            }
            else if(a0 == b1){
                c[ci++] = a0;
                bi += 2;
                if(bi + 1 >= bSize) break;
                b0 = b[bi]; b1 = b[bi+1];
                continue;
            }
            else if(a1 == b0){
                c[ci++] = b0;
                ai += 2;
                if(ai + 1 >= aSize) break;
                a0 = a[ai]; a1 = a[ai+1];
                continue;
            }
            if(a1 == b1){
                c[ci++] = a1;
                ai += 2;
                bi += 2;
                if(ai + 1 >= aSize) break;
                if(bi + 1 >= bSize) break;
                a0 = a[ai]; a1 = a[ai+1];
                b0 = b[bi]; b1 = b[bi+1];
            }
            else if(a1 > b1){
                bi += 2;
                if(bi + 1 >= bSize) break;
                b0 = b[bi]; b1 = b[bi+1];
            }
            else {
                ai += 2;
                if(ai + 1 >= aSize) break;
                a0 = a[ai]; a1 = a[ai+1];
            }
        }
        return new Result(ai, bi, ci);
    }

    private static int serial(long[] a, int ai, int aSize, long[] b, int bi, int bSize, long[] c, int ci)
    {
        if(ai == aSize || bi == bSize)
            return ci;
        for(;;){
            long a0 = a[ai], b0 = b[bi];
            if (a0 == b0) {
                c[ci++] = a0;
                ai += 1;
                bi += 1;
                if (ai == aSize || bi == bSize)
                    break;
            }
            else if (a0 < b0) {
                ai += 1;
                if (ai == aSize) break;
            }
            else {
                bi += 1;
                if (bi == bSize) break;
            }
        }
        return ci;
    }

    public static int intersect(long[] a, int aSize, long[] b, int bSize, long[] c)
    {
        int ai = 0, bi = 0, ci = 0;
        return serial(a, ai, aSize, b, bi, bSize, c, ci);
    }

    private static int serial(int[] a, int ai, int aSize, int[] b, int bi, int bSize, int[] c, int ci)
    {
        if(ai == aSize || bi == bSize)
            return ci;
        for(;;){
            int a0 = a[ai], b0 = b[bi];
            if (a0 == b0) {
                c[ci++] = a0;
                ai += 1;
                bi += 1;
                if (ai == aSize || bi == bSize)
                    break;
            }
            else if (a0 < b0) {
                ai += 1;
                if (ai == aSize) break;
            }
            else {
                bi += 1;
                if (bi == bSize) break;
            }
        }
        return ci;
    }

    public static int intersect(int[] a, int aSize, int[] b, int bSize, int[] c)
    {
        int ai = 0, bi = 0, ci = 0;
        /*if(aSize >= 2 && bSize >= 2){
            Result r = blockwise2(a, aSize, b, bSize, c);
            ai = r.ai;
            bi = r.bi;
            ci = r.ci;
        }*/
        return serial(a, ai, aSize, b, bi, bSize, c, ci);
    }
}

