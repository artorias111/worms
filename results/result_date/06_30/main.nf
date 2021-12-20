date = new Date().format( 'yyyyMMdd' )
params.out = "TELOMERE-${date}"
params.bam = "/projects/b1059/data/c_elegans/WI/alignments/*.{bam,bam.bai}"
//params.bam = "/projects/b1059/projects/Shriram/worm/data/test_bam/*{.bam,bam.bai}"

Channel.fromFilePairs(params.bam)
       .map{it -> [it[0], it[1][0], it[1][1]]}
       .set{bam_individual}

process call_telseq{
    publishDir "${params.out}/individual", mode: 'copy'
    cpus 4
    tag{SM}
    input:
        set val(SM), file("${SM}.bam"), file("${SM}.bam.bai") from bam_individual
    output:
        file("telseq_out.txt") into results

    """
        telseq -z TTAGGC -H -m -u ${SM}.bam > telseq_out.txt
    """
}

process combine_telseq{
    publishDir "${params.out}", mode: 'copy'
    input:
        file("ind_telseq?.txt") from results.toSortedList()
    output:
        file("telseq.tsv")

    '''
        telseq -h > telseq.tsv
        cat ind_telseq*.txt >> telseq.tsv
    '''
}
